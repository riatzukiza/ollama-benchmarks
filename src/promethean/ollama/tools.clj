(ns promethean.ollama.tools
  "Tool registry with OpenAI-style schema generation and parameter validation."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.java.io :as io])
  (:import
    (java.io File)))

;; Tool registry state
(defonce ^:private !tools (atom {}))
(defonce ^:private !tool-id-counter (atom 0))
(defonce ^:private !tool-index (atom {}))

;; Helper functions
(defn generate-tool-id []
  (swap! !tool-id-counter inc))

(defn build-tool-index [tool-name]
  (swap! !tool-index update-in [tool-name] (count (:tools @!tool-index))))

(defn clear-tool-index []
  (reset! !tool-index {}))

;; Type mapping for Clojure to JSON schema
(def ^:private type->json-type
  {int "integer"
   float "number"
   string "string"
   bool "boolean"
   keyword "string"
   map "object"
   vec "array"
   any "string"})

(defn clojure-type->json-type [clojure-type]
  (case clojure-type
    :int "integer"
    :float "number"
    :long "integer"
    :double "number"
    :string "string"
    :bool "boolean"
    :keyword "string"
    :map "object"
    :vec "array"
    :set "array"
    :any "string"))

(defn generate-ollama-schema [tool-map]
  "Generate OpenAI-compatible JSON schema from tool definition."
  (let [params (:tool/parameters tool-map)
        schema {:type "function"
                  :function {:name (:tool/name tool-map)
                           :description (:tool/description tool-map)}
                  :parameters (generate-parameters-schema params)}]
    (json/generate-string schema)))

(defn generate-parameters-schema [params]
  (into {:type "object"
          :properties (reduce-kv 
                        (fn [result param-name param-info]
                          (let [param-type (:type param-info)
                                json-type (clojure-type->json-type param-type)]
                            (assoc result param-name
                                   {:type json-type
                                    :description (get param-info :description)
                                    (default (get param-info :default)}))
                        {}))
                      params}))

;; Tool Registry Protocol
(defprotocol ToolRegistry
  "Protocol for tool registration and lookup."
  (register-tool! [this tool] "Register tool by unique name")
  (tools [] "Return all registered tools")
  (tool-by-name [this name] "Lookup tool by name")
  (tools-by-tag [this tag] "Find tools by capability tag")
  (clear-tools! [] "Clear all tools")
  (get-tool-count [] "Get count of registered tools")
  (get-tool-index [] "Get tool index for search"))

;; Tool Registry Implementation
(defrecord ToolRegistryImpl []
  ToolRegistry
  (!tools [this])
  (!tool-id-counter [this])
  (!tool-index [this]))

(defn register-tool!
  "Register a tool with validation and schema generation."
  [this tool-map]
  (let [tool-name (:tool/name tool-map)
        tool-id (generate-tool-id)]
    (when-let [existing-tool (get @!tools tool-name)]
      (throw (ex-info "Tool already registered" {:tool-name tool-name})))
    (swap! !tools assoc tool-name tool-map)
    (build-tool-index tool-name)
    tool-id))

(defn tools
  "Return all registered tools."
  [this]
  (vals @!tools))

(defn tool-by-name
  "Lookup tool by name."
  [this tool-name]
  (get @!tools tool-name))

(defn tools-by-tag
  "Find tools by capability tag."
  [this tag]
  (filter #(contains? (:tool/tags %) tag)
           (vals @!tools)))

(defn clear-tools!
  "Clear all registered tools."
  [this]
  (reset! !tools {})
    (reset! !tool-id-counter 0)
    (clear-tool-index))

(defn get-tool-count
  "Get count of registered tools."
  [this]
  (count @!tools))

(defn get-tool-index
  "Get tool index for search."
  [this]
  @!tool-index)

(defn create-registry []
  "Create new tool registry instance."
  (->ToolRegistryImpl))