(ns ollama.tools
  "Tool registry + OpenAI-style schema + argument validation (spec.alpha).

  You define tools via def-tool:

    (def-tool mul
      (doc \"Multiply two ints\")
      (domain :math)
      (tags :math :mul)
      (params
        [a :int \"Left\"]
        [b :int \"Right\"])
      (impl [{:keys [a b]}] (* a b))
      (bench
        (benchcase \"mul/7x9\"
          (prompt \"Compute 7 times 9.\")
          (args {:a 7 :b 9})
          (final-contains \"63\")
          (no-extras))))

  The same tools file can be loaded by:
  - benchmark runner
  - production agents"
  (:require
    [cheshire.core :as json]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]))

;; ---------------------------
;; Registry
;; ---------------------------

(defonce ^:private !tools (atom {}))

(defn clear-tools! [] (reset! !tools {}))
(defn tools [] (->> @!tools vals (sort-by :name) vec))
(defn tool-by-name [nm] (get @!tools (str nm)))

(defn register-tool!
  "Registers tool map by :name. Returns tool."
  [tool]
  (when-not (string? (:name tool))
    (throw (ex-info "Tool :name must be a string" {:tool tool})))
  (swap! !tools assoc (:name tool) tool)
  tool)

;; ---------------------------
;; Type system (tiny)
;; ---------------------------

(def ^:private type->pred
  {:int int?
   :integer int?
   :number number?
   :float number?
   :double number?
   :string string?
   :bool boolean?
   :boolean boolean?
   :keyword keyword?
   :map map?
   :object map?
   :vec vector?
   :vector vector?
   :array sequential?
   :any (constantly true)})

(def ^:private type->json
  {:int "integer"
   :integer "integer"
   :number "number"
   :float "number"
   :double "number"
   :string "string"
   :bool "boolean"
   :boolean "boolean"
   :keyword "string"
   :map "object"
   :object "object"
   :vec "array"
   :vector "array"
   :array "array"
   :any "string"})

(defn- kwish [x]
  (cond
    (keyword? x) x
    (symbol? x) (keyword (name x))
    (string? x) (keyword x)
    :else (keyword (str x))))

(defn normalize-tool-name [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (name x)
    (map? x) (:name x)
    :else (str x)))

(defn- parse-param
  "Param vector grammar:
    [a :int]
    [a :int \"desc\"]
    [a :int {:desc \"...\" :optional true}]
    [a :int \"desc\" {:optional true}]"
  [v]
  (let [[k t a b] v
        k (kwish k)
        t (kwish t)
        opts (cond
               (map? a) a
               (map? b) b
               :else {})
        desc (cond
               (string? a) a
               (string? b) b
               :else (or (:desc opts) ""))]
    {:key k
     :type t
     :desc desc
     :optional? (boolean (:optional opts))}))

(defn- params->schema+spec
  "Turns params into:
   - JSON schema (OpenAI tool schema shape)
   - spec object validating argument maps"
  [params]
  (let [ps (mapv parse-param params)
        required (->> ps (remove :optional?) (mapv (comp name :key)))
        allowed-keys (set (map :key ps))
        validators (into {}
                         (map (fn [{:keys [key type]}]
                                (let [pred (get type->pred type (constantly true))]
                                  {key pred}))
                         ps))
        required-keys (set (map :key (remove :optional? ps)))

        args-spec
        (s/and
          map?
          ;; required keys present
          (fn [m] (every? #(contains? m %) required-keys))
          ;; no unknown keys
          (fn [m] (every? (fn [[k _]] (contains? allowed-keys (kwish k))) m))
          ;; values pass type preds
          (fn [m]
            (every? (fn [[k v]]
                      (let [k* (kwish k)
                            pred (get validators k* (constantly true))]
                        (pred v)))
                    m)))

        schema
        {"type" "object"
         "additionalProperties" false
         "properties"
          (into {}
                (map (fn [{:keys [key type desc]}]
                       {(name key) 
                        (cond-> {"type" (get type->json type "string")}
                          (seq desc) (assoc "description" desc))}))
                ps)
          "required" required}]
    {:args-spec args-spec
     :schema schema}))

(defn coerce-arguments
  "Ollama may return arguments as a map or a JSON string.
   Returns a keyword-keyed map when possible."
  [args]
  (cond
    (nil? args) {}
    (map? args) (into {} (map (fn [[k v]] [(kwish k) v]) args))
    (string? args)
    (try
      (let [m (json/parse-string args true)]
        (if (map? m)
          (into {} (map (fn [[k v]] [(kwish k) v]) m))
          {}))
      (catch Exception _ {}))
    :else {}))

(defn validate-tool-call
  "Validates {:name \"tool\" :arguments <map|json-string>} using tool args-spec."
  [{:keys [name arguments]}]
  (let [tool (tool-by-name name)]
    (cond
      (nil? tool)
      {:ok false :error :unknown-tool :details {:name name}}

      :else
      (let [args (coerce-arguments arguments)
            spec (:args-spec tool)]
        (if (s/valid? spec args)
          {:ok true :arguments args}
          {:ok false
           :error :invalid-arguments
           :details {:name name
                     :arguments args
                     :explain (s/explain-str spec args)}})))))

(defn invoke-tool!
  "Invokes tool implementation. Returns {:ok true :value ...} or {:ok false ...}."
  [tool-name arguments]
  (let [tool (tool-by-name tool-name)]
    (if-not tool
      {:ok false :error :unknown-tool :details {:name tool-name}}
      (try
        (let [f (:impl tool)
              v (f (coerce-arguments arguments))]
          {:ok true :value v})
        (catch Throwable t
          {:ok false :error :tool-exception
           :details {:name tool-name
                     :message (.getMessage t)
                     :class (str (class t))}})))))

(defn tool->ollama-schema
  "Converts tool map to OpenAI-style schema Ollama accepts."
  [tool]
  {:type "function"
   :function {:name (:name tool)
              :description (:description tool)
              :parameters (:parameters tool)}})

(defn tools->ollama-schemas []
  (mapv tool->ollama-schema (tools)))

;; -------------------------------------------------------
;; Bench template DSL (optional metadata on tools)
;; -------------------------------------------------------

(defmacro prompt [s] `(hash-map :prompt ~s))
(defmacro system [s] `(hash-map :system ~s))
(defmacro messages [xs] `(hash-map :messages ~xs))
(defmacro args [m] `(hash-map :args ~m))
(defmacro arguments-subset [m] `(hash-map :arguments-subset ~m))
(defmacro policy [p] `(hash-map :policy ~p))
(defmacro no-extras [] `(hash-map :no_extras true))
(defmacro final-contains [s] `(hash-map :final {:contains ~s}))
(defmacro final-regex [r] `(hash-map :final {:regex ~r}))
(defmacro with-tools [& ts] `(hash-map :tools ~(vec (map normalize-tool-name ts))))
(defmacro penalties [{:keys [wrong_first extra_calls]}]
  `(hash-map :wrong_first_penalty ~wrong_first
             :extra_calls_penalty ~extra_calls))

(defmacro benchcase
  "Tool-owned benchmark template. Stored under :bench/prompt-templates.
  Usage:

    (benchcase \"mul/7x9\"
      (prompt \"Compute 7 times 9.\")
      (args {:a 7 :b 9})
      (final-contains \"63\")
      (no-extras))"
  [id & forms]
  (let [pairs (vec forms)]
    `(merge {:id ~id} ~@pairs)))

(defmacro bench
  "Directive payload for def-tool. Just returns vector of benchcase maps."
  [& cases]
  `(hash-map :bench/prompt-templates ~(vec cases)))

;; -------------------------------------------------------
;; def-tool (the fun part)
;; -------------------------------------------------------

(defn- directive? [x sym]
  (and (seq? x) (= (first x) sym)))

(defmacro def-tool
  "Define + register a tool.

  Directives:
    (doc \"...\")
    (domain :math)
    (tags :a :b)
    (params [a :int \"...\"] [b :int \"...\"] ...)
    (impl [args] ...)
    (bench (benchcase ...) ...)

  Returns a var bound to the registered tool map."
  [tool-name & body]
  (let [nm (name tool-name)
        ;; parse directives
        doc-str (or (some #(when (string? %) %) body)
                    (some #(when (directive? % 'doc) (second %)) body)
                    "")
        domain-kw (some #(when (directive? % 'domain) (second %)) body)
        tags-set (some #(when (directive? % 'tags) (set (rest %))) body)
        params-form (some #(when (directive? % 'params) (rest %)) body)
        impl-form (some #(when (or (directive? % 'impl) (directive? % 'run)) %) body)
        bench-form (some #(when (directive? % 'bench) % ) body)

        {:keys [args-spec schema]}
        (params->schema+spec (or params-form []))

        impl-fn
        (when impl-form
          (let [[_ argv & impl-body] impl-form]
            `(fn ~argv ~@impl-body)))

        bench-map
        (when bench-form
          (second (macroexpand-1 bench-form)))]

    `(def ~tool-name
       (register-tool!
         (merge
           {:name ~nm
            :description ~doc-str
            :domain ~domain-kw
            :tags ~(or tags-set #{})
            :parameters ~schema
            :args-spec ~args-spec
            :impl ~(or impl-fn `(fn [_#] (throw (ex-info "Tool has no impl" {:tool ~nm}))))}
           ~bench-map)))))

;; Helper macros for directive parsing
(defmacro doc [s] s)
(defmacro domain [k] k)
(defmacro tags [& _] nil)
(defmacro params [& _] nil)
(defmacro impl [& _] nil)
(defmacro run [& xs] `(impl ~@xs))