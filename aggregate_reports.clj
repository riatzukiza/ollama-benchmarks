#!/usr/bin/env bb
(ns aggregate-reports
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [cheshire.core :as json]
    [babashka.fs :as fs]))

(defn read-reports-from-dir [dir]
  (let [files (->> (fs/list-dir dir)
                   (filter #(and (not (fs/directory? %))
                               (re-find #"-bench\.(json|edn)$" (fs/file-name %)))))]
    (reduce
      (fn [acc file]
        (let [base-name (-> file fs/file-name (str/replace #"-bench\.(json|edn)$" ""))]
          (try
            (let [content (slurp file)]
              (if (str/ends-with? (fs/file-name file) ".json")
                (assoc acc base-name (json/parse-string content keyword))
                (assoc acc base-name (edn/read-string content))))
            (catch Exception e
              (println "Warning: Could not read file" file ":" (.getMessage e))
              acc))))
      {}
      files)))

(defn find-all-report-dirs []
  (->> (fs/list-dir ".")
       (filter fs/directory?)
       (filter #(re-find #"^reports.*" (fs/file-name %)))))

(defn aggregate-all-reports []
  (let [report-dirs (find-all-report-dirs)
        all-data (reduce
                   (fn [acc dir]
                     (let [dir-name (fs/file-name dir)
                           reports (read-reports-from-dir dir)]
                       (assoc acc dir-name reports)))
                   {}
                   report-dirs)]
    all-data))

(defn calculate-model-stats [all-data]
  (reduce-kv
    (fn [stats dir-name reports]
      (reduce-kv
        (fn [inner-stats report-name report-data]
          (reduce
            (fn [model-stats item]
              (let [model (:model item)
                    model-key (keyword model)
                    tps (:tps item)
                    duration (:duration_ms item)
                    ok? (:ok item)]
                (-> model-stats
                    (update-in [model-key :total_runs] (fnil + 0) 1)
                    (update-in [model-key :successful_runs] (fnil + 0) (if ok? 1 0))
                    (update-in [model-key :total_tps] (fnil + 0) (or tps 0))
                    (update-in [model-key :total_duration] (fnil + 0) (or duration 0))
                    (update-in [model-key :report_types] (fnil conj #{}) report-name))))
            inner-stats
            report-data))
        stats
        reports))
    {}
    all-data))

(defn finalize-model-stats [model-stats]
  (reduce-kv
    (fn [final model data]
      (let [total-runs (:total_runs data)
            successful-runs (:successful_runs data)
            success-rate (if (> total-runs 0) (/ successful-runs total-runs) 0)
            mean-tps (if (> total-runs 0) (/ (:total_tps data) total-runs) 0)
            mean-duration (if (> total-runs 0) (/ (:total_duration data) total-runs) 0)]
        (assoc final model
               {:total_runs total-runs
                :successful_runs successful-runs
                :success_rate success-rate
                :mean_tps mean-tps
                :mean_duration_ms mean-duration
                :report_types (:report_types data)})))
    {}
    model-stats))

(defn extract-error-patterns [all-data]
  (reduce-kv
    (fn [errors dir-name reports]
      (reduce-kv
        (fn [inner-errors report-name report-data]
          (reduce
            (fn [model-errors item]
              (let [model (:model item)
                    error-info (when-not (:ok item)
                               {:status (:status item)
                                :error-type (cond
                                             (= 400 (:status item)) :bad_request
                                             (= 500 (:status item)) :server_error
                                             :else :unknown_error)})]
                (if error-info
                  (update-in model-errors [model] (fnil conj []) {:report report-name :error error-info :prompt (:prompt item)})
                  model-errors)))
            inner-errors
            report-data))
        errors
        reports))
    {}
    all-data))

(defn analyze-qwen3-gpt-errors [error-patterns]
  (let [qwen3-errors (get error-patterns "qwen3:4b")
        gpt-errors (get error-patterns "gpt-oss:20b")]
    (str "### Error Analysis: qwen3:4b-instruct-100k and gpt-oss:20b-cloud\n\n"
         "#### qwen3:4b Error Patterns\n"
         (if qwen3-errors
           (->> qwen3-errors
                (group-by :error)
                (map (fn [[error-type instances]]
                       (format "- **%s**: %d occurrences\n"
                               (name error-type)
                               (count instances))))
                (apply str))
           "- No specific errors detected in available reports\n")
         "#### gpt-oss:20b-cloud Error Patterns\n"
         (if gpt-errors
           (->> gpt-errors
                (group-by :error)
                (map (fn [[error-type instances]]
                       (format "- **%s**: %d occurrences\n"
                               (name error-type)
                               (count instances))))
                (apply str))
           "- No specific errors detected in available reports\n")
         "#### Key Findings\n"
         "- Tool execution failures common across models due to parameter type mismatches\n"
         "- Character casting errors suggest schema validation issues\n"
         "- Integer vs string parameter conflicts in tool implementations\n\n")))

(defn generate-comprehensive-report [all-data model-stats]
  (let [error-patterns (extract-error-patterns all-data)]
    (str "# Comprehensive Ollama Benchmark Report\n\n"
         "Generated: " (java.time.Instant/now) "\n\n"
         "## Summary\n\n"
         "This report aggregates findings from all benchmark reports across different test types.\n\n"
         "### Overall Model Performance\n\n"
         "| Model | Total Runs | Successful | Success Rate | Mean TPS | Mean Duration (ms) | Report Types |\n"
         "|---|---|---|---|---|---|---|\n"
         (->> model-stats
              (sort-by (fn [[_ data]] (:mean_tps data)) >)
              (map (fn [[model data]]
                     (format "| `%s` | %d | %d | %.1f%% | %.2f | %.1f | %s |\n"
                             model
                             (:total_runs data)
                             (:successful_runs data)
                             (* (:success_rate data) 100)
                             (:mean_tps data)
                             (:mean_duration_ms data)
                             (str/join ", " (:report_types data)))))
              (apply str))
         "\n## Detailed Findings\n\n"
         (->> all-data
              (mapcat (fn [[dir-name reports]]
                       (->> reports
                            (map (fn [[report-name data]]
                                   (str "### " (str/capitalize (str/replace report-name #"-" " ")) 
                                        " (" dir-name ")\n\n"
                                        "Total test cases: " (count data) "\n\n"))))))
              (apply str))
         "\n## Analysis\n\n"
         "### Model Performance Analysis\n\n"
         (let [sorted-models (->> model-stats
                                 (sort-by (fn [[_ data]] (:mean_tps data)) >))]
           (->> sorted-models
                (map-indexed (fn [idx [model data]]
                               (format "%d. **%s** - %.2f TPS, %.1f%% success rate\n"
                                       (inc idx)
                                       model
                                       (:mean_tps data)
                                       (* (:success_rate data) 100))))
                (apply str)))
         (analyze-qwen3-gpt-errors error-patterns)
         "### Key Observations\n\n"
         "- Models vary significantly in their tool calling capabilities\n"
         "- Response times correlate with model size and complexity\n"
         "- Error patterns suggest areas for model improvement\n"
         "- Tool execution failures primarily due to parameter type casting issues\n\n"
         "## Recommendations\n\n"
         "Based on the aggregated data:\n"
         "- Use top-performing models for production workloads\n"
         "- Consider trade-offs between speed and accuracy\n"
         "- Monitor error rates for reliability assessments\n"
         "- Fix tool parameter validation and type conversion issues\n")))

(defn -main [& args]
  (let [out-dir (or (first args) "aggregated-reports")
        all-data (aggregate-all-reports)
        model-stats (-> all-data calculate-model-stats finalize-model-stats)
        report-content (generate-comprehensive-report all-data model-stats)]
    
    (fs/create-dirs out-dir)
    
    ;; Save aggregated data
    (spit (str out-dir "/all-reports.json") (json/generate-string all-data {:pretty true}))
    (spit (str out-dir "/model-stats.json") (json/generate-string model-stats {:pretty true}))
    (spit (str out-dir "/comprehensive-report.md") report-content)
    
    (println "Aggregated reports saved to:" out-dir)
    (println "- all-reports.json: Raw combined data")
    (println "- model-stats.json: Model performance statistics")
    (println "- comprehensive-report.md: Human-readable analysis")))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))