(ns promethean.ollama.agents
  "Agent registry + supervisor + runner loop.
  
  def-agent is meant to feel like OpenAI's Agent SDK:
    - model
    - instructions
    - tools
    - options
    - think
  
  You can run agents in production OR in benchmarks."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.core.async :as a]
    [promethean.ollama.client :as client]
    [promethean.ollama.tools :as tools]
    [promethean.ollama.bus :as bus]))

;; Agent registry
(defonce ^:private !agents (atom {}))

(defn register-agent!
  "Registers agent map by :name. Returns agent."
  [agent]
  (when-not (string? (:name agent))
    (throw (ex-info "Agent :name must be a string" {:agent agent})))
  (swap! !agents assoc (:name agent) agent)
  agent)

(defn agents []
  "Return all registered agents sorted by name."
  (->> @!agents vals (sort-by :name) vec))

(defn agent-by-name [nm]
  (get @!agents (str nm)))

(defn clear-agents! []
  (reset! !agents {}))

;; Agent specification validation
(s/def ::name string?)
(s/def ::model string?)
(s/def ::instructions string?)
(s/def ::think boolean?)
(s/def ::options map?)
(s/def ::tools (s/coll-of string? :kind vector?))
(s/def ::max-steps pos-int?)
(s/def ::timeout-ms pos-int?)

(s/def ::agent
  (s/keys :req-un [::name ::model]
          :opt-un [::instructions ::think ::options ::tools ::max-steps ::timeout-ms]))

(defn normalize-tool-ref [x]
  (tools/normalize-tool-name x))

(defn agent-tools->schemas [agent]
  (let [names (set (or (:tools agent) []))
        subset (filter (fn [t] (contains? names (:name t))) (tools/tools))]
    (mapv tools/tool->ollama-schema subset)))

;; Runtime: tool loop
(defn- assistant->tool-calls [assistant-message]
  (or (:tool_calls assistant-message) []))

(defn- extract-call [tool-call]
  (let [f (:function tool-call)
        id (or (:id tool-call)
               (:tool_call_id tool-call)
               (:id f)
               (:tool_call_id f))]
    {:id (when (some? id) (str id))
     :name (:name f)
     :arguments (:arguments f)
     :raw tool-call}))

(defn- tool-result-message [{:keys [name tool_call_id content]}]
  (cond-> {:role "tool"
            :name name
            :tool_name name
            :tool_call_id tool_call_id
            :content content}
     (some? tool_call_id) (assoc :tool_call_id tool_call_id)))

(defn- stringify [x]
  (if (string? x) x (pr-str x)))

(defn run!
  "Run an agent conversation until no tool calls or max-steps.
  
  Input:
    {:agent <agent map or agent name string>
     :host ...
     :messages [...]
     :tools <override tool schemas>
     :max-steps ...
     :timeout-ms ...}
  
  Returns:
    {:messages [...]
     :steps [...]
     :final_assistant {...}
     :stopped_reason :no_tool_calls | :max_steps}"
  [{:keys [agent host messages tools max-steps timeout-ms]}]
  (let [agent-map (cond
                    (string? agent) (agent-by-name agent)
                    (map? agent) agent
                    :else nil)]
        _ (when-not (s/valid? ::agent agent-map)
            (throw (ex-info "Invalid agent map"
                            {:explain (s/explain-str ::agent agent-map)})))

        model (:model agent-map)
        think (:think agent-map)
        options (:options agent-map)
        max-steps (long (or max-steps (:max-steps agent-map) 4))
        timeout-ms (long (or timeout-ms (:timeout-ms agent-map) 300000))

        system-msg (when (seq (:instructions agent-map))
                     {:role "system" :content (:instructions agent-map)})

        tools-schemas (or tools
                        (when (seq (:tools agent-map))
                          (agent-tools->schemas agent-map))
                        (tools/tools->ollama-schemas))

        initial (cond-> []
                    (some? system-msg) (conj system-msg)
                    true (into (vec messages))]

    (loop [msgs initial
           steps []
           i 0]
      (let [resp (client/chat! {:host host
                                 :model model
                                 :messages msgs
                                 :tools tools-schemas
                                 :think think
                                 :options options
                                 :timeout-ms timeout-ms})
            assistant (:message resp)
            msgs' (conj msgs assistant)
            tool-calls (mapv extract-call (assistant->tool-calls assistant))]

        (if (or (empty? tool-calls) (>= i (dec max-steps)))
          {:messages msgs'
           :steps steps
           :final_assistant assistant
           :raw_last_response resp
           :stopped_reason (if (empty? tool-calls) :no_tool_calls :max_steps)}
          
          ;; Execute tools
          (let [tool-results
                (mapv (fn [{:keys [name arguments]}]
                          (let [valid (tools/validate-tool-call {:name name :arguments arguments})
                                invoked (when (:ok valid)
                                          (tools/invoke-tool! name (:arguments valid)))]
                            (assoc (select-keys valid [:ok :arguments]) :invoked invoked)))
                        tool-calls)]

            ;; Add tool result messages
            (let [tool-messages (mapv tool-result-message tool-results)
                  msgs-with-tools (into msgs' tool-messages)]
              
              (recur msgs-with-tools
                     (conj steps {:i i
                                   :assistant assistant
                                   :tool_calls tool-calls
                                   :tool_results tool-results})
                     (inc i)))))))))

;; Agent supervisor coordination
(defn- create-supervisor
  "Create supervisor that manages multiple child agents."
  [config]
  {:supervisor true
   :children (atom #{})
   :config config})

(defn- spawn-agent
  "Spawn a child agent under supervisor."
  [{:keys [supervisor agent-config]}]
  (let [agent-id (str (:name (:supervisor supervisor)) "-" (random-uuid))
        agent-map (merge agent-config {:name agent-id})]
    (swap! (:children supervisor) conj agent-id)
    (register-agent! agent-map)
    agent-id))

(defn- stop-agent
  "Stop a specific agent."
  [agent-id]
  (when-let [agent (agent-by-name agent-id)]
    (swap! !agents dissoc agent-id)))

;; def-agent macro (simplified for now)
(defmacro def-agent
  "Define + register an agent.
  
  Directives:
    (model \"qwen3\")
    (instructions \"...\")
    (tools mul add)
    (options {:temperature 0})
    (think true)
    (max-steps 4)
    (timeout-ms 300000)
  
  Returns var bound to agent map."
  [agent-name & body]
  (let [nm (name agent-name)
        model (or (some #(and (seq? %) (= (first %) 'model) %) body) "gpt-3")
        instructions (or (some #(and (seq? %) (= (first %) 'instructions) %) body) "")
        tools-form (some #(and (seq? %) (= (first %) 'tools) %) body)
        tools-names (when tools-form (vec (rest tools-form)))
        think (or (some #(and (seq? %) (= (first %) 'think) %) body) false)
        options (or (some #(and (seq? %) (= (first %) 'options) %) body) {})
        max-steps (or (some #(and (seq? %) (= (first %) 'max-steps) %) body) 4)
        timeout-ms (or (some #(and (seq? %) (= (first %) 'timeout-ms) %) body) 300000)]

    `(def ~agent-name
       (register-agent!
         (merge
           {:name ~nm
            :model ~model
            :instructions ~instructions
            :tools ~tools-names
            :think ~think
            :options ~options
            :max-steps ~max-steps
            :timeout-ms ~timeout-ms}
           ~@body)))))