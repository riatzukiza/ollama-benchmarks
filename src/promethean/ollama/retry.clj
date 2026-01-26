(ns promethean.ollama.retry
  "Retry utilities for API calls with exponential backoff."
  (:require [clojure.tools.logging :as log]))

(defn exponential-backoff
  "Calculate exponential backoff delay with jitter."
  [attempt {:keys [base-delay-ms max-delay-ms multiplier jitter?]
            :or {base-delay-ms 100 max-delay-ms 30000 multiplier 2.0 jitter? true}}]
  (let [raw-delay (* base-delay-ms (Math/pow multiplier attempt))
        delay (min raw-delay max-delay-ms)]
    (if jitter?
      (+ delay (* delay 0.1 (rand)))
      delay)))

(defn retryable-error?
  "Check if error is retryable."
  [error]
  (or (and (instance? java.io.IOException error)
           (re-find #"Connection refused|Network is unreachable|Connection reset" 
                    (.getMessage error)))
      (and (instance? java.net.SocketTimeoutException error))
      (and (instance? java.net.ConnectException error))
      (and (map? error)
           (contains? error :error)
           (or (= "try again" (:error error))
               (= "timeout" (:error error))))))

(defn retry-with-backoff
  "Execute a function with exponential backoff retry logic.
   
   Options:
   - :max-attempts (default 3) - Maximum number of attempts
   - :base-delay-ms (default 100) - Initial delay
   - :max-delay-ms (default 30000) - Maximum delay cap
   - :multiplier (default 2.0) - Backoff multiplier
   - :jitter? (default true) - Add random jitter
   - :on-retry (fn [attempt error]) - Callback on each retry"
  [f {:keys [max-attempts base-delay-ms max-delay-ms multiplier jitter? on-retry]
      :or {max-attempts 3
           base-delay-ms 100
           max-delay-ms 30000
           multiplier 2.0
           jitter? true}
      :as opts}]
  (loop [attempt 0]
    (let [execute-try (fn []
                       (try
                         (let [result (f)]
                           (if (and (map? result) (contains? result :error))
                             ;; Handle API-style error responses
                             (let [error (ex-info "API error" result)]
                               (if (and (< attempt (dec max-attempts)) (retryable-error? error))
                                 {:retry true :error error}
                                 {:retry false :error error}))
                             ;; Success
                             {:retry false :result result}))
                         (catch Exception e
                           (let [error e]
                             (if (and (< attempt (dec max-attempts)) (retryable-error? error))
                               {:retry true :error error}
                               {:retry false :error error})))))
          outcome (execute-try)]
      (if (:retry outcome)
        (do
          (when on-retry
            (on-retry attempt (:error outcome)))
          (let [delay-ms (exponential-backoff attempt opts)]
            (log/warn "Retrying in" delay-ms "ms"
                     {:attempt (inc attempt) :max-attempts max-attempts})
            (Thread/sleep delay-ms)
            (recur (inc attempt))))
        (if-let [error (:error outcome)]
          (throw error)
          (:result outcome))))))

(defn retry-handler
  "Create a retry handler for specific error types."
  [error-pred {:keys [max-attempts base-delay-ms max-delay-ms multiplier jitter?]
               :or {max-attempts 3
                    base-delay-ms 100
                    max-delay-ms 30000
                    multiplier 2.0
                    jitter? true}}]
  (fn [f]
    (retry-with-backoff f 
                      {:max-attempts max-attempts
                       :base-delay-ms base-delay-ms
                       :max-delay-ms max-delay-ms
                       :multiplier multiplier
                       :jitter? jitter?
                       :on-retry (fn [attempt error]
                                    (log/warn "Retry attempt" (inc attempt) 
                                             "due to" (type error)
                                             ":" (.getMessage error)))})))