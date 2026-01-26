(ns ollama.client
  "Shared Ollama client helpers for benchmarks.

  Implements non-streaming /api/chat call and returns parsed JSON response.
  Benchmarks can share this namespace instead of re-implementing HTTP + JSON.

  Docs:
  - Tool calling: https://docs.ollama.com/capabilities/tool-calling"
  (:require
    [clj-http.client :as http]
    [cheshire.core :as json]
    [clojure.string :as str]
    [promethean.ollama.retry :as retry]
    [promethean.ollama.errors :as errors]))

(def ^:private default-timeout-ms 300000) ;; 5 minutes
(def default-host "http://localhost:11434")

(defn- normalize-host [host]
  (cond
    (nil? host) default-host
    (str/blank? host) default-host
    :else (str/replace host #"/+$" "")))

(defn- post-json-request!
  "Internal POST function without retry logic."
  [{:keys [host path body timeout-ms]
    :or {host default-host
         timeout-ms default-timeout-ms}}]
  (let [url (str (normalize-host host) path)
        response (http/post url {:body (json/generate-string body)
                              :headers {"content-type" "application/json"}
                              :throw-exceptions false
                              :connect-timeout timeout-ms
                              :max-time timeout-ms})
        {:keys [status body]} response]
    (when (or (nil? status) (not (<= 200 status 299)))
      (throw (ex-info "Ollama HTTP error"
                      {:status status
                       :path path
                       :body body})))
    (json/parse-string body true)))

(defn- post-json!
  "POST a JSON body to an Ollama endpoint and parse JSON response into a Clojure map.
   Returns parsed response with keyword keys."
  [{:keys [host path body timeout-ms retry-config]
    :or {host default-host
         timeout-ms default-timeout-ms}}]
  (let [retry-opts (merge {:max-attempts 3
                            :base-delay-ms 200
                            :max-delay-ms 10000
                            :multiplier 1.5
                            :jitter? true}
                           retry-config)]
    (retry/retry-with-backoff
      #(post-json-request! {:host host
                           :path path
                           :body body
                           :timeout-ms timeout-ms})
      (merge retry-opts
             {:on-retry (fn [attempt error]
                          (errors/log-error error 
                                          {:operation :ollama/http-post
                                           :path path
                                           :attempt (inc attempt)
                                           :host host}))}))))

(defn chat!
  "Call Ollama /api/chat with retry logic.

  opts:
  - :host          (default http://localhost:11434)
  - :model         (required)
  - :messages      (required) [{:role \"user\" :content \"...\"} ...]
  - :tools         (optional) ollama tool schemas [{:type \"function\" :function {...}} ...]
  - :stream        (default false) (this helper is non-streaming)
  - :think         (optional true/false for models that support it)
  - :options       (optional map) passed to Ollama (temperature, seed, etc)
  - :timeout_ms    (default 5 minutes)
  - :retry-config  (optional) retry configuration map

  Returns parsed JSON response.
  The assistant message is usually at (:message resp)."
  [{:keys [host model messages tools stream think options timeout_ms retry_config]
    :or {host default-host
         stream false}}]
  (when (true? stream)
    (throw (ex-info "This benchmark client currently supports stream=false only."
                    {:hint "Use a streaming client if you need chunked tool_calls."})))
  (when (or (nil? model) (str/blank? (str model)))
    (throw (ex-info "Missing required :model" {})))
  (when-not (sequential? messages)
    (throw (ex-info "Missing/invalid :messages (must be a vector/list)" {:messages messages})))
  (post-json! {:host host
               :path "/api/chat"
               :timeout_ms timeout_ms
               :retry_config retry_config
               :body (cond-> {:model model
                              :messages messages
                              :stream false}
                       (some? tools) (assoc :tools tools)
                       (some? think) (assoc :think think)
                       (some? options) (assoc :options options))}))

;; ---- Legacy compatibility helpers ----

(defn now-ms []
  (System/currentTimeMillis))

(defn mean [xs]
  (when (seq xs)
    (/ (reduce + xs) (double (count xs)))))