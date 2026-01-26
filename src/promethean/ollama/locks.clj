(ns promethean.ollama.locks
  "File/resource lock service with TTL support and conflict resolution."
  (:require
    [clojure.java.io :as io]))

;; Lock state
(defonce ^:private !locks (atom {}))
(defonce ^:private !lock-id-counter (atom 0))

(defn generate-lock-id []
  (str "lock-" (swap! !lock-id-counter inc)))

(defn acquire!
  "Acquire lock on resource. Returns {:ok true} or {:ok false :owner ...}."
  [{:keys [path mode owner ttl-ms]}]
  (let [lock-id (generate-lock-id)
        timestamp (System/currentTimeMillis)
        ttl (or ttl-ms 60000)] ; default 60 seconds TTL
    (locking !locks
      (let [existing-lock (get @!locks path)]
        (cond
          (nil? existing-lock)
          (let [new-lock {:id lock-id
                        :path path
                        :mode mode
                        :owner owner
                        :since timestamp
                        :ttl-ms ttl
                        :heartbeat timestamp}]
            (swap! !locks assoc path new-lock)
            {:ok true :lock-id lock-id})

          (and (= (:owner existing-lock) owner)
               (<= (- timestamp (:since existing-lock)) (:ttl-ms existing-lock)))
          ;; Same owner refreshing lock
          (let [refreshed-lock (assoc existing-lock :since timestamp :heartbeat timestamp)]
            (swap! !locks assoc path refreshed-lock)
            {:ok true :lock-id (:id existing-lock)})

          :else
          ;; Resource locked by someone else
          {:ok false 
           :locked-by (:owner existing-lock)
           :locked-since (:since existing-lock)
           :lock-id (:id existing-lock)})))))

(defn release!
  "Release lock by owner. Returns true if successful, false if not owner or doesn't exist."
  [{:keys [path owner lock-id]}]
  (locking !locks
    (when-let [current-lock (get @!locks path)]
      (if (and current-lock
               (= (:owner current-lock) owner)
               (or (nil? lock-id) (= lock-id (:id current-lock))))
        (do
          (swap! !locks dissoc path)
          true)
        false))))

(defn heartbeat!
  "Extend lock TTL by updating heartbeat timestamp."
  [{:keys [path owner lock-id]}]
  (locking !locks
    (when-let [current-lock (get @!locks path)]
      (if (and current-lock
               (= (:owner current-lock) owner)
               (or (nil? lock-id) (= lock-id (:id current-lock))))
        (let [timestamp (System/currentTimeMillis)
              updated-lock (assoc current-lock :heartbeat timestamp)]
          (swap! !locks assoc path updated-lock)
          true)
        false))))

(defn get-status
  "Get current lock status for path."
  [path]
  (get @!locks path))

(defn get-all-locks
  "Get all active locks."
  @!locks)

(defn cleanup-expired-locks!
  "Remove all locks that have exceeded their TTL."
  []
  (let [timestamp (System/currentTimeMillis)
        locks @!locks]
    (doseq [[path lock] locks]
      (when (> (- timestamp (:since lock)) (+ (:ttl-ms lock) 5000)) ; 5s grace period
        (println "Removing expired lock on" path "by" (:owner lock))
        (swap! !locks dissoc path)))))

(defn find-conflicts
  "Find all conflicts for given resource requests."
  [resource-requests]
  (let [conflicts (atom [])]
    (doseq [[i req1] resource-requests]
      (doseq [[j req2] resource-requests]
        (when (and (< i j)
                   (= (:path req1) (:path req2))
                   (not= (:mode req1) (:mode req2)))
          (let [status1 (acquire! req1)
                status2 (acquire! req2)]
            (when (and (:ok status1) (:ok status2))
              ;; Both would succeed in isolation - this is a conflict
              (swap! conflicts conj {:resources [req1 req2] :type :resource-mode-conflict}))))
    @conflicts))

;; Public API for lock manager
(defn create-lock-manager
  "Create lock manager with automatic cleanup."
  []
  ;; Start cleanup thread
  (future
    (try
      (while true
        (Thread/sleep 5000) ; Check every 5 seconds
        (cleanup-expired-locks!))
      (catch InterruptedException _
        (println "Lock cleanup thread stopped")))))

(defn check-lock-health
  "Return health information about lock service."
  []
  (let [locks @!locks
        timestamp (System/currentTimeMillis)]
    {:total-locks (count locks)
     :expired-locks (count (filter #(> (- timestamp (:since %)) (:ttl-ms %)) locks))
     :oldest-lock (when (seq locks) 
                       (apply min (map :since locks)))
     :memory-usage (estimate-memory-usage locks)}))

(defn- estimate-memory-usage [locks]
  "Estimate memory usage of lock data structures."
  (* (count locks) 200)) ; Rough estimate per lock

;; Lock validation utilities
(defn valid-lock-path?
  "Validate that path is safe for locking."
  [path]
  (and (string? path)
           (not (str/blank? path))
           (not (str/includes? path ".."))
           (not (re-find #"^[/\\]$" path))))