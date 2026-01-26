(ns promethean.ollama.events
  "Event sourcing and logging for state reconstruction, debugging, and resumable execution."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [cheshire.core :as json])
  (:import
    (java.io File)
    (java.nio.file Files OpenOption StandardOpenOption)
    (java.nio.file.attribute FileAttribute)))

;; Event types
(def event-types
  #{:agent/spawned
    :agent/status
    :agent/message
    :tool/requested
    :tool/result
    :lock/acquired
    :lock/released
    :plan/loaded
    :plan/updated
    :task/state
    :bench/case-start
    :bench/case-end
    :system/state})

;; Event log configuration
(defonce ^:private !event-log-path (atom "logs/events.jsonl"))
(defonce ^:private !event-lock (atom (Object.)))

(defn set-event-log-path! [path]
  (reset! !event-log-path path))

(defn write-event!
  "Write event to JSONL log file with timestamp."
  [event-data]
  (when-not (contains? event-types (:type event-data))
    (throw (ex-info "Unknown event type" {:event event-data})))
  (let [timestamp (System/currentTimeMillis)
        event-line (json/generate-string (merge event-data {:timestamp timestamp}))
        path @!event-log-path]
    (.mkdirs (.getParent (File. path)))
    (locking !event-lock
      (try
        (let [file-path (java.nio.file.Paths/get path)
              exists? (.exists (java.nio.file.Files file-path))
              writer (if exists?
                       (java.nio.file.Files/newBufferedWriter file-path 
                                                (into-array OpenOption [StandardOpenOption/APPEND])
                                                java.nio.charset.StandardCharsets/UTF_8)
                       (java.nio.file.Files/newBufferedWriter file-path 
                                                (into-array OpenOption [StandardOpenOption/CREATE 
                                                                     StandardOpenOption/APPEND])
                                                java.nio.charset.StandardCharsets/UTF_8))]
          (.write writer (str event-line "\n"))
          (.flush writer)
          (.close writer))
        (catch Exception e
          (println "Failed to write event:" (.getMessage e))))))

(defn write-events!
  "Write multiple events to log."
  [events]
  (doseq [event events]
    (write-event! event)))

(defn read-events-from-file
  "Read and parse all events from JSONL file."
  [path]
  (when-not (.exists (File. path))
    [])
  (let [lines (str/split-lines (slurp path))]
    (keep identity 
            (map (fn [line]
                   (when (seq line)
                     (try
                       (json/parse-string line true)
                       (catch Exception e
                         (println "Failed to parse event line:" line)
                         nil))))
                 lines)))))

(defn reduce-events
  "Reconstruct application state from event sequence."
  [events]
  (reduce (fn [state event]
            (case (:type event)
              :agent/spawned (update state :agents conj {:agent/id (:agent/id event) :status :initializing})
              :agent/status (update-in state [:agents (:agent/id event)] assoc :agent/status (:status event))
              :tool/requested (update state :current-tools (:tool/name event))
              :tool/result (update-in state [:tool-results (:tool/name event)] assoc (:tool/name event) (:ok event) (:result event))
              :bench/case-start (update state :current-case event)
              :bench/case-end (update state :completed-cases conj event)
              state)) ; default: return state unchanged
          {} events))

(defn get-agent-state
  "Get current state for specific agent."
  [agent-id events]
  (reduce (fn [state event]
            (if (= (:agent/id event) agent-id)
              (case (:type event)
                :agent/spawned (assoc state :agent/id agent-id :status :initializing)
                :agent/status (assoc state :agent/id agent-id :status (:status event))
                state)
              state))
          {} events))

(defn get-active-tool-calls
  "Get all tool calls that haven't completed yet."
  [events]
  (->> events
       (filter #(= (:type %) :tool/requested))
       (map #(select-keys % [:agent/id :tool/name :arguments :timestamp]))))

(defn get-completed-tool-calls
  "Get all completed tool calls with results."
  [events]
  (->> events
       (filter #(= (:type %) :tool/result))
       (map #(select-keys % [:agent/id :tool/name :ok :value :timestamp]))))

;; Public API
(defn create-event-log!
  "Initialize event log file and ensure it's writable."
  [path]
  (.mkdirs (.getParent (File. path)))
  (when-not (.exists (File. path))
    (spit path ""))

(defn benchmark-state-snapshot
  "Create snapshot of benchmark execution state."
  [events]
  {:total-events (count events)
   :cases-started (count (filter #(= (:type %) :bench/case-start) events))
   :cases-completed (count (filter #(= (:type %) :bench/case-end) events))
   :active-agents (->> events
                   (filter #(= (:type %) :agent/spawned)
                   (map :agent/id)
                   (distinct)
                   (count))
   :tool-requests (get-active-tool-calls events)
   :tool-results (get-completed-tool-calls events)})