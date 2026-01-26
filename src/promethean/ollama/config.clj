(ns promethean.ollama.config
  "Centralized configuration management with environment variable support, validation, and runtime updates."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    (java.io File)
    (java.nio.file Files Paths)))

;; Configuration hierarchy and defaults
(def default-config
  {:ollama/default-host "http://localhost:11434"
   :ollama/default-model "qwen3:7b"
   :ollama/default-timeout-ms 300000
   :events/log-path "logs/events.jsonl"
   :events/enable-async true
   :locks/default-ttl-ms 60000
   :locks/max-concurrent 10
   :bus/channel-size 1000
   :validation/strict-mode false
   :validation/spec-alpha true})

;; Runtime configuration state
(defonce ^:private !runtime-config (atom nil))

;; Environment variable mappings
(def env-mappings
  {"OLLAMA_HOST" :ollama/default-host
   "OLLAMA_MODEL" :ollama/default-model
   "OLLAMA_TIMEOUT" :ollama/default-timeout-ms
   "OLLAMA_LOG_PATH" :events/log-path
   "OLLAMA_LOCK_TTL" :locks/default-ttl-ms
   "OLLAMA_MAX_LOCKS" :locks/max-concurrent
   "OLLAMA_BUS_SIZE" :bus/channel-size
   "OLLAMA_STRICT_VALIDATION" :validation/strict-mode
   "OLLAMA_SPEC_ALPHA" :validation/spec-alpha})

(defn- load-env-config!
  "Load configuration from environment variables."
  []
  (reduce-kv 
    (fn [config [env-key config-key]]
      (if-let [value (System/getenv env-key)]
        (assoc config config-key
               (case config-key
                 (:ollama/default-host :ollama/default-model) (keyword value)
                 ((:ollama/default-timeout-ms :locks/default-ttl-ms :locks/max-concurrent :bus/channel-size) 
                  (if (re-find #"^\d+$" value) 
                    (Long/parseLong value) 
                    value))
                 (#{:validation/strict-mode :validation/spec-alpha} (not= "false" value))))
        config))
    default-config
    env-mappings))

(defn validate-config!
  "Validate configuration values and throw descriptive errors."
  [config]
  (when-not (string? (:ollama/default-host config))
    (throw (ex-info "Invalid Ollama host" {:config-key :ollama/default-host :value (:ollama/default-host config)})))
  (when-not (pos? (:ollama/default-timeout-ms config))
    (throw (ex-info "Invalid timeout value" {:config-key :ollama/default-timeout-ms :value (:ollama/default-timeout-ms config)})))
  (when-not (pos? (:locks/default-ttl-ms config))
    (throw (ex-info "Invalid lock TTL" {:config-key :locks/default-ttl-ms :value (:locks/default-ttl-ms config)})))
  (when-not (pos? (:locks/max-concurrent config))
    (throw (ex-info "Invalid max concurrent locks" {:config-key :locks/max-concurrent :value (:locks/max-concurrent config)})))
  config)

(defn get-config
  "Get current runtime configuration."
  []
  (or @!runtime-config default-config))

(defn get
  "Get specific configuration value."
  [config-key]
  (clojure.core/get (get-config) config-key))

(defn update-config!
  "Update configuration value at runtime with validation."
  [config-key new-value]
  (let [old-config (get-config)
        new-config (assoc old-config config-key new-value)
        validated-config (validate-config! new-config)]
    (reset! !runtime-config validated-config)
    (println "Configuration updated:" config-key "=" new-value)))

(defn save-config!
  "Save current configuration to file."
  [file-path]
  (let [config (get-config)]
    (.mkdirs (.getParent (File. file-path)))
    (spit file-path (pr-str config))))

(defn load-config!
  "Load configuration from file."
  [file-path]
  (when (.exists (File. file-path))
    (let [loaded-config (read-string (slurp file-path))
          validated-config (validate-config! loaded-config)]
      (reset! !runtime-config validated-config)
      (println "Configuration loaded from file:" file-path))))

;; Configuration access helpers for specific components
(defn ollama-config
  "Get Ollama-related configuration."
  []
  (select-keys (get-config) [:ollama/default-host :ollama/default-model :ollama/default-timeout-ms]))

(defn events-config
  "Get events-related configuration."
  []
  (select-keys (get-config) [:events/log-path :events/enable-async]))

(defn locks-config
  "Get lock-related configuration."
  []
  (select-keys (get-config) [:locks/default-ttl-ms :locks/max-concurrent]))

(defn bus-config
  "Get message bus configuration."
  []
  (select-keys (get-config) [:bus/channel-size]))

(defn validation-config
  "Get validation-related configuration."
  []
  (select-keys (get-config) [:validation/strict-mode :validation/spec-alpha]))

;; Configuration validation helpers
(defn pos?
  "Check if value is a positive number."
  [x]
  (and (number? x) (pos-int? x)))

(defn valid-host?
  "Check if value is a valid HTTP URL."
  [host]
  (and (string? host)
       (or (str/starts-with? host "http://")
           (str/starts-with? host "https://"))))