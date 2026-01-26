(ns promethean.ollama.tools.examples
  "Example tools demonstrating the tool registry system."
  (:require
    [promethean.ollama.tools :as tools]))

;; Register example tools
(defn register-example-tools!
  "Register a collection of example tools for testing."
  []
  (tools/register-tool! 
    {:tool/name "math/add"
     :tool/description "Add two integers"
     :tool/domain :math
     :tool/tags #{:arithmetic :deterministic}
     :tool/parameters {:a {:type "integer" :required true}
                            :b {:type "integer" :required true}}
     :tool/implementation (fn [{:keys [a b]}] (+ a b))})
  (tools/register-tool! 
    {:tool/name "math/multiply"
     :tool/description "Multiply two numbers"
     :tool/domain :math
     :tool/tags #{:arithmetic :deterministic}
     :tool/parameters {:a {:type "number" :required true}
                            :b {:type "number" :required true}}
     :tool/implementation (fn [{:keys [a b]}] (* a b))})
  (tools/register-tool! 
    {:tool/name "math/divide"
     :tool/description "Divide two numbers"
     :tool/domain :math
     :tool/tags #{:arithmetic :deterministic}
     :tool/parameters {:a {:type "number" :required true}
                            :b {:type "number" :required true}}
     :tool/implementation (fn [{:keys [a b]}] (if (zero? b) 
                                             (throw (ex-info "Division by zero" {:a a :b b}))
                                             (/ a b))})
  (tools/register-tool! 
    {:tool/name "string/concat"
     :tool/description "Concatenate two strings"
     :tool/domain :text
     :tool/tags #{:string-manipulation :deterministic}
     :tool/parameters {:s1 {:type "string" :required true}
                            :s2 {:type "string" :required true}}
     :tool/implementation (fn [{:keys [s1 s2]}] (str s1 s2))})
  (tools/register-tool! 
    {:tool/name "validate-email"
     :tool/description "Validate email format"
     :tool/domain :validation
     :tool/tags #{:validation :deterministic}
     :tool/parameters {:email {:type "string" :required true}}
     :tool/implementation (fn [{:keys [email]}] 
                        (re-find #".+@.+\..+" email))})
  (tools/register-tool! 
    {:tool/name "generate-uuid"
     :tool/description "Generate a random UUID"
     :tool/domain :system
     :tool/tags #{:deterministic :side-effect-free}
     :tool/parameters {}
     :tool/implementation (fn [] (str (random-uuid)))}))

(defn lookup-example!
  "Demonstrate tool lookup functionality."
  []
  (println "Looking up math/add tool:")
  (if-let [add-tool (tools/tool-by-name "math/add")]
    (println "Found:" add-tool)
    (println "  Parameters:" (:tool/parameters add-tool))
    (println "  Description:" (:tool/description add-tool)))
  (println "\nAll tools:" (tools/tools))
  (println "Tools by tag :arithmetic:" (tools/tools-by-tag :arithmetic))
  (println "Tool count:" (tools/get-tool-count)))

(defn tools-by-tag-example!
  "Demonstrate tag-based tool filtering."
  [tag]
  (let [tagged-tools (tools/tools-by-tag tag)]
    (println "\nTools with tag" tag ":")
    (doseq [tool tagged-tools]
      (println "  -" (:tool/name tool)))))

(defn tool-count-example!
  "Demonstrate tool count tracking."
  []
  (println "Total tools registered:" (tools/get-tool-count))
  (println "Active tools count:" (count (tools/tools))))