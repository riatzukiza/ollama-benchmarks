(ns promethean.ollama.errors
  "Error handling and mitigation for Ollama API operations."
  (:require
    [clojure.string :as str]
    [clojure.tools.logging :as log]))

;; Error classification
(defn classify-error
  "Classify an error into categories for appropriate handling."
  [error]
  (let [ex-data (ex-data error)
        cause (.getCause error)
        message (.getMessage error)]
    (cond
      ;; Network connectivity issues
      (or (instance? java.net.SocketTimeoutException error)
          (instance? java.net.SocketTimeoutException cause)
          (instance? java.net.ConnectException error)
          (instance? java.net.ConnectException cause)
          (instance? java.net.SocketException error)
          (instance? java.net.SocketException cause))
      {:type :network
       :retryable? true
       :severity :medium
       :description "Network connectivity issue"}
      
      ;; HTTP status codes
      (some? (:status ex-data))
      (let [status (:status ex-data)]
        (cond
          (<= 200 status 299)
          {:type :success :retryable? false :severity :none}
          
          (= status 400)
          {:type :client-error :retryable? false :severity :high
           :description "Bad request - check input format"}
          
          (= status 401)
          {:type :auth-error :retryable? false :severity :high
           :description "Authentication failed"}
          
          (= status 403)
          {:type :permission-error :retryable? false :severity :high
           :description "Permission denied"}
          
          (= status 404)
          {:type :not-found :retryable? false :severity :medium
           :description "Resource not found"}
          
          (= status 429)
          {:type :rate-limit :retryable? true :severity :medium
           :description "Rate limit exceeded"}
          
          (<= 500 status 599)
          {:type :server-error :retryable? true :severity :high
           :description "Server error"}
          
          :else
          {:type :unknown-http :retryable? false :severity :medium
           :description (str "Unexpected HTTP status: " status)}))
      
      ;; Ollama-specific error patterns
      (some? (:body ex-data))
      (let [body (:body ex-data)
            body-str (if (string? body) body (str body))]
        (cond
(.contains body-str "model loading")
           {:type :model-loading :retryable? true :severity :medium
            :description "Model is still loading"}
           
           (.contains body-str "resource unavailable")
           {:type :resource-unavailable :retryable? true :severity :medium
            :description "Resource temporarily unavailable"}
           
           (.contains body-str "out of memory")
           {:type :out-of-memory :retryable? false :severity :high
            :description "Server out of memory"}
           
           (.contains body-str "model not found")
           {:type :model-not-found :retryable? false :severity :high
            :description "Model not found on server"}
           
           (.contains body-str "invalid")
          {:type :invalid-request :retryable? false :severity :medium
           :description "Invalid request format"}
          
          :else
          {:type :ollama-error :retryable? false :severity :medium
           :description "Ollama API error"}))
      
      ;; Default classification
      :else
      {:type :unknown :retryable? false :severity :medium
       :description "Unknown error type"})))

(defn parse-ollama-error
  "Parse Ollama error response for detailed information."
  [error]
  (let [ex-data (ex-data error)
        classification (classify-error error)
        {:keys [status body path]} ex-data]
    (merge classification
           {:status status
            :body body
            :path path
            :original-error error
            :timestamp (System/currentTimeMillis)})))

(defn suggest-mitigation
  "Suggest mitigation strategies based on error classification."
  [error-info]
  (case (:type error-info)
    :network
    ["Check network connectivity"
     "Verify Ollama server is running"
     "Try different endpoint"
     "Increase timeout"]
    
    :rate-limit
    ["Wait before retrying"
     "Reduce request frequency"
     "Use smaller batches"
     "Consider rate limiting"]
    
    :model-loading
    ["Wait for model to finish loading"
     "Try a different model"
     "Check server resources"]
    
    :resource-unavailable
    ["Retry after a short delay"
     "Check server resource usage"
     "Reduce request complexity"]
    
    :out-of-memory
    ["Use smaller model"
     "Reduce context size"
     "Restart Ollama server"]
    
    :model-not-found
    ["Check model name spelling"
     "Pull the model first: ollama pull <model>"
     "List available models: ollama list"]
    
    :client-error
    ["Check request format"
     "Validate input parameters"
     "Review API documentation"]
    
    :auth-error
    ["Check authentication setup"
     "Verify API keys if required"
     "Check server configuration"]
    
    :permission-error
    ["Check file/directory permissions"
     "Run with appropriate user"
     "Verify server access controls"]
    
    :server-error
    ["Check Ollama server logs"
     "Restart Ollama service"
     "Report issue if persistent"]
    
    ["Check error details"
     "Review logs"
     "Contact support if needed"]))

(defn create-error-report
  "Create a detailed error report for logging and debugging."
  [error context]
  (let [error-info (parse-ollama-error error)
        mitigations (suggest-mitigation error-info)]
    {:error/error-id (str "ERR-" (System/currentTimeMillis))
     :error/type (:type error-info)
     :error/severity (:severity error-info)
     :error/description (:description error-info)
     :error/retryable? (:retryable? error-info)
     :error/status (:status error-info)
     :error/body (:body error-info)
     :error/path (:path error-info)
     :error/mitigations mitigations
     :error/context context
     :error/timestamp (:timestamp error-info)}))

(defn log-error
  "Log error with appropriate level and context."
  [error context]
  (let [report (create-error-report error context)]
    (case (:error/severity report)
      :none (log/debug "Error report" report)
      :low (log/info "Error report" report)
      :medium (log/warn "Error report" report)
      :high (log/error "Error report" report))
    report))

(defn handle-error
  "Handle error with logging and mitigation suggestions."
  [error context]
  (let [report (log-error error context)]
    (log/info "Suggested mitigations:" (str/join ", " (:error/mitigations report)))
    report))

;; Error recovery strategies
(defn recovery-strategy
  "Return appropriate recovery strategy for error type."
  [error-info]
  (case (:type error-info)
    :network :retry-with-backoff
    :rate-limit :exponential-backoff
    :model-loading :wait-and-retry
    :resource-unavailable :short-delay-retry
    :server-error :limited-retry
    :client-error :fail-fast
    :auth-error :fail-fast
    :permission-error :fail-fast
    :model-not-found :fail-fast
    :out-of-memory :fail-fast
    :unknown :fail-fast))

(defn should-retry?
  "Determine if operation should be retried based on error."
  [error attempt max-attempts]
  (let [error-info (classify-error error)]
    (and (:retryable? error-info)
         (< attempt max-attempts)
         (not= (:severity error-info) :high))))

;; Convenience functions for common error scenarios
(defn is-transient-error?
  "Check if error is transient (network, server, rate limit)."
  [error]
  (let [error-info (classify-error error)]
    (contains? #{:network :rate-limit :server-error :model-loading :resource-unavailable}
               (:type error-info))))

(defn is-permanent-error?
  "Check if error is permanent (client, auth, permission)."
  [error]
  (let [error-info (classify-error error)]
    (contains? #{:client-error :auth-error :permission-error :model-not-found :out-of-memory}
               (:type error-info))))

(defn get-retry-delay
  "Get appropriate retry delay for error type."
  [error attempt]
  (let [error-info (classify-error error)]
    (case (:type error-info)
      :network (* 200 (Math/pow 1.5 attempt))
      :rate-limit (* 1000 (Math/pow 2 attempt))
      :model-loading (* 500 (Math/pow 1.2 attempt))
      :resource-unavailable (* 300 (Math/pow 1.3 attempt))
      :server-error (* 500 (Math/pow 2 attempt))
      1000))) ; Default 1 second