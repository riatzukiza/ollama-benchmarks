(ns promethean.ollama.resources
  "Centralized resource management and tracking for the entire framework."
  (:require
    [promethean.ollama.config :as config]
    [promethean.ollama.events :as events]))

;; Resource types
(def resource-types
  #{:file-lock :tool-execution :llm-call :memory-usage :cpu-usage})

;; Resource state tracking
(defonce ^:private !resource-state (atom {:allocations {} :limits {}}))

(defn allocate-resource!
  "Allocate a resource of given type with tracking."
  [resource-type resource-id owner options]
  (let [allocation-id (random-uuid)
        timestamp (System/currentTimeMillis)
        allocation {:id allocation-id
                   :type resource-type
                   :resource-id resource-id
                   :owner owner
                   :since timestamp
                   :options options}]
    (swap! !resource-state 
             update-in [:allocations resource-id resource-type] 
             assoc allocation-id allocation)))
    (events/write-event! 
      {:type :resource/allocated
       :resource-type resource-type
       :resource-id resource-id
       :allocation-id allocation-id
       :owner owner
       :options options})
    allocation-id))

(defn release-resource!
  "Release a previously allocated resource."
  [allocation-id]
  (when-let [[resource-id resource-type] 
               (first (filter (fn [[_ alloc]] 
                                   (= (:id alloc) allocation-id))
                                   (get-in @!resource-state [:allocations resource-id]))]
    (swap! !resource-state 
             update-in [:allocations resource-id] 
             dissoc allocation-id))
    (events/write-event!
      {:type :resource/released
       :resource-type resource-type
       :resource-id resource-id
       :allocation-id allocation-id})))

(defn get-resource-allocations
  "Get all current allocations for a resource type."
  [resource-type]
  (get-in @!resource-state [:allocations resource-type]))

(defn get-allocation-info
  "Get specific allocation information."
  [allocation-id]
  (some (fn [[_ alloc]]
            (when (= (:id alloc) allocation-id)
              alloc))
          (for [[_ _ allocations] (vals @!resource-state)]
            allocations)))

(defn check-resource-limits!
  "Check if allocating would exceed resource limits."
  [resource-type request-owner]
  (let [current-allocations (get-resource-allocations resource-type)
        current-count (count current-allocations)
        max-allowed (get-in @!resource-state [:limits resource-type])]
    (when (>= current-count max-allowed)
      (throw (ex-info "Resource limit exceeded" 
                      {:resource-type resource-type
                       :current current-count
                       :max-allowed max-allowed
                       :requester request-owner}))))

(defn set-resource-limit!
  "Set maximum allowed allocations for a resource type."
  [resource-type max-allocations]
  (swap! !resource-state assoc-in [:limits resource-type] max-allocations)
  (events/write-event!
      {:type :resource/limit-updated
       :resource-type resource-type
       :new-limit max-allocations}))

(defn get-resource-stats
  "Get usage statistics for all resource types."
  []
  (reduce-kv 
    (fn [stats resource-type allocations]
      (let [current-count (count allocations)
            peak-count (get-in @!resource-state [:peak-counts resource-type] 0)]
        (assoc stats resource-type {:current current-count :peak peak-count})))
    {}
    (:allocations @!resource-state)))

;; Resource usage monitoring helpers
(defn track-peak-usage!
  "Track peak usage for resource type."
  [resource-type count]
  (let [current-peak (get-in @!resource-state [:peak-counts resource-type] 0)
        new-peak (max current-peak count)]
    (swap! !resource-state assoc-in [:peak-counts resource-type] new-peak)))

(defn cleanup-expired-resources!
  "Clean up resources that have exceeded their TTL."
  []
  (let [now (System/currentTimeMillis)
        expired-allocations 
        (filter (fn [[_ alloc]]
                  (let [ttl (get-in alloc [:options :ttl-ms])]
                    (and ttl (< now (+ (:since alloc) ttl)))))
                (for [[_ _ allocations] (vals @!resource-state)]
                  allocations))]
    (doseq [alloc expired-allocations]
      (release-resource! (:id alloc)))))

;; Resource conflict detection
(defn detect-resource-conflicts
  "Detect potential conflicts between resources."
  [resource-type]
  (let [allocations (get-resource-allocations resource-type)]
    (filter (fn [[id1 alloc1]]
              (some (fn [[id2 alloc2]]
                      (and (not= id1 id2)
                           (resource-conflict? alloc1 alloc2)))
                    allocations))
            allocations)))

(defn- resource-conflict?
  "Check if two resource allocations conflict."
  [alloc1 alloc2]
  (and (= (:type alloc1) (:type alloc2))
       (or (and (= (:type alloc1) :file-lock)
                (= (:resource-id alloc1) (:resource-id alloc2)))
           (and (= (:type alloc1) :tool-execution)
                (some #(= (:resource-id %) (:resource-id alloc2)) 
                       (get-in alloc1 [:options :exclusive-resources])))))

;; Configuration-based resource limits
(defn init-resource-limits!
  "Initialize resource limits from configuration."
  []
  (let [cfg (config/get-config)]
    (set-resource-limit! :file-lock (:locks/max-concurrent cfg))
    (set-resource-limit! :tool-execution (:tool/max-concurrent cfg))
    (set-resource-limit! :llm-call (:llm/max-concurrent cfg))
    (set-resource-limit! :memory-usage (:memory/max-mb cfg))
    (set-resource-limit! :cpu-usage (:cpu/max-percent cfg))))

;; Periodic cleanup task
(defn start-resource-monitor!
  "Start background resource monitoring and cleanup."
  []
  (future
    (while true
      (Thread/sleep 30000) ; Check every 30 seconds
      (cleanup-expired-resources!)
      (let [stats (get-resource-stats)]
        (events/write-event! 
          {:type :resource/stats-updated
           :stats stats
           :timestamp (System/currentTimeMillis)})))))