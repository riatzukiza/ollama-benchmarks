(ns promethean.ollama.client
  "Thin Ollama /api/chat wrapper using Java HttpClient.
   No extra deps besides cheshire.

   The goal: benchmarks and real agents share the same runtime."
  (:require
    [cheshire.core :as json]
    [clojure.string :as str]
    [promethean.ollama.retry :as retry]
    [promethean.ollama.errors :as errors])
  (:import
    (java.net URI)
    (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
    (java.time Duration)))

(def default-host "http://localhost:11434")

(defn- uri [host path]
  (URI/create (str (if (string? host) host default-host) path)))

(defn- http-client ^HttpClient []
  (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofSeconds 15))
      (.build)))

(defn- chat-request!
  "Internal chat request function without retry logic."
  [{:keys [host model messages tools options timeout-ms think]
    :or {host default-host
         timeout-ms 300000}}]
  (let [body (cond-> {:model model
                       :stream false
                       :messages messages}
                (seq tools) (assoc :tools tools)
                (map? options) (assoc :options options)
                (some? think) (assoc :think think))
        req (-> (HttpRequest/newBuilder)
                (.uri (uri host "/api/chat"))
                (.timeout (Duration/ofMillis (long timeout-ms)))
                (.header "Content-Type" "application/json")
                (.POST (HttpRequest$BodyPublishers/ofString
                        (json/generate-string body)))
                (.build))
        resp (.send (http-client) req (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)
        raw (.body resp)]
    (if (<= 200 status 299)
      (json/parse-string raw true)
      (throw (ex-info "Ollama /api/chat failed"
                      {:status status
                       :body raw
                       :request body})))))

(defn chat!
  "Calls Ollama /api/chat (non-stream) with retry logic.
   Input:
     {:host \"http://localhost:11434\"
      :model \"qwen3\"
      :messages [{:role \"user\" :content \"hi\"}]
      :tools [...]              ;; tool schemas (OpenAI-style)
      :options {...}            ;; Ollama options
      :timeout-ms 300000
      :think true|false
      :retry-config {...}}      ;; optional retry configuration

   Returns decoded JSON response map."
  [{:keys [host model messages tools options timeout-ms think retry-config]
    :or {host default-host
         timeout-ms 300000}}]
  (let [retry-opts (merge {:max-attempts 3
                           :base-delay-ms 200
                           :max-delay-ms 10000
                           :multiplier 1.5
                           :jitter? true}
                          retry-config)]
    (retry/retry-with-backoff
      #(chat-request! {:host host
                       :model model
                       :messages messages
                       :tools tools
                       :options options
                       :timeout-ms timeout-ms
                       :think think})
      (merge retry-opts
             {:on-retry (fn [attempt error]
                          (errors/log-error error 
                                          {:operation :ollama/chat
                                           :attempt (inc attempt)
                                           :model model
                                           :host host}))}))))

(defn- normalize-model-name [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (name x)
    (map? x) (:name x)
    :else (str x)))