(ns promethean.ollama.bus
  "Message bus architecture for async communication between framework components."
  (:require
    [clojure.core.async :as a]))

;; Message types
(def message-types
  #{:agent/lifecycle
    :tool/request
    :tool/result
    :bench/control
    :system/state
    :agent/communication
    :resource/request})

;; Message routing configuration
(defonce ^:private !router (atom {}))

(defn register-handler!
  "Register handler function for specific message type."
  [message-type handler-fn]
  (swap! !router assoc message-type handler-fn))

(defn unregister-handler!
  "Remove handler for message type."
  [message-type]
  (swap! !router dissoc message-type))

(defn route-message
  "Route message to appropriate handler based on type."
  [message]
  (when-let [handler (get @!router (:type message))]
    (handler message)))

(defn create-bus
  "Create new message bus with default handlers."
  []
  (let [bus (a/chan 1000)]
    ;; Default handlers that just log messages
    (register-handler! :agent/lifecycle (fn [msg] (println "Agent lifecycle:" msg)))
    (register-handler! :tool/request (fn [msg] (println "Tool request:" msg)))
    (register-handler! :tool/result (fn [msg] (println "Tool result:" msg)))
    (register-handler! :bench/control (fn [msg] (println "Benchmark control:" msg)))
    (register-handler! :system/state (fn [msg] (println "System state:" msg)))
    (register-handler! :agent/communication (fn [msg] (println "Agent communication:" msg)))
    (register-handler! :resource/request (fn [msg] (println "Resource request:" msg)))
    {:channel bus}))

(defn publish!
  "Publish message to all subscribers."
  [bus message]
  (a/put! (:channel bus) message))

(defn subscribe!
  "Subscribe to messages of specific type."
  [bus message-type]
  (a/go-loop []
    (let [[msg port] (a/alts! [(:channel bus)])]
      (when (= (:type msg) message-type)
        (route-message msg))
      (recur))))

(defn create-request-response-pair
  "Create request/response channel pair for RPC-style communication."
  []
  (let [req-chan (a/chan 10)
        resp-chan (a/chan 10)]
    {:request req-chan :response resp-chan}))

(defn send-request!
  "Send request and wait for response with timeout."
  [{:keys [request-chan response-chan timeout-ms]} message timeout-ms]
  (a/put! request-chan message)
  (a/go-loop []
    (let [[value port] (a/alts! [response-chan] :default (or timeout-ms 5000))]
      (when (some? value)
        value)
      (when (= port :timeout)
        ::timeout))))

;; Utility functions
(defn create-filtered-bus
  "Create bus that filters messages based on predicate."
  [predicate]
  (let [source (a/chan 1000)
        filtered (a/chan 1000)]
    (a/pipeline
      [(a/filter predicate) source]
      [filtered])
    {:source source :filtered filtered}))

(defn broadcast-to-type!
  "Send message to all handlers of specific type."
  [bus message-type message]
  (when-let [handler (get @!router message-type)]
    (handler message)))

(defn close-bus!
  "Close all channels in message bus."
  [bus]
  (a/close! (:channel bus))
  (when (:filtered bus)
    (a/close! (:filtered bus)))
  (doseq [[_ handler] @!router]
    (when (and (fn? handler) (contains? (methods handler) :close))
      (handler))))