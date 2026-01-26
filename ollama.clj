(ns ollama
  (:require
    [clojure.tools.cli :as cli]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clj-http.client :as http]
    [cheshire.core :as json]))

;; ---- Common IO helpers ----

(defn read-edn-file [f]
  (edn/read-string (slurp f)))

(defn now-ms []
  (System/currentTimeMillis))

(defn ensure-dir! [d]
  (.mkdirs (java.io.File. d))
  d)

(defn write-edn! [f data]
  (spit f (pr-str data)))

(defn write-json! [f data]
  (spit f (json/generate-string data {:pretty true})))

(defn write-md! [f s]
  (spit f s))

;; ---- Common CLI parsing ----

(def cli-spec
  {:config    {:coerce :string :desc "EDN config file with models/prompts"}
   :out-dir   {:coerce :string :desc "Output directory for reports"}
   :n         {:coerce :long   :desc "Number of runs per (model,prompt) combo"}
   :tools     {:coerce :string :desc "EDN file with tool definitions"}
   :session-id {:coerce :string :desc "Session ID for cumulative runs (auto-generated if not provided)"}
   :resume    {:coerce :bool   :desc "Resume from existing session"}})

(defn parse-args [args]
  (let [parsed (cli/parse-opts args cli-spec)
        opts (first parsed)]
    (when-not (:config opts)
      (println "Missing --config <file>")
      (System/exit 1))
    (when-not (:out-dir opts)
      (println "Missing --out-dir <dir>")
      (System/exit 1))
    (update opts :n #(or % 3))))

(defn parse-args-with-tools [args]
  (let [parsed (cli/parse-opts args cli-spec)
        opts (first parsed)]
    (when-not (:config opts)
      (println "Missing --config <file>")
      (System/exit 1))
    (when-not (:out-dir opts)
      (println "Missing --out-dir <dir>")
      (System/exit 1))
    (update opts :n #(or % 3))))

;; ---- Common Ollama API functions ----

(defn ollama-chat
  [{:keys [endpoint model prompt tools]}]
  (let [url (str (or endpoint "http://localhost:11434") "/api/chat")
        body {:model model
              :messages [{:role "user" :content prompt}]
              :stream false}
        body-with-tools (if tools
                        (assoc body :tools tools)
                        body)
        {:keys [status body]} (curl/post url {:body (json/generate-string body-with-tools)
                                              :headers {"content-type" "application/json"}
                                              :throw-exceptions false})]
    (if (= 200 status)
      (json/parse-string body keyword)
      {:error true :status status :body body})))

(defn mean [xs]
  (when (seq xs)
    (/ (reduce + xs) (double (count xs)))))

;; ---- Common report generation ----

(defn md-table [results]
  (let [header "| Model | Prompt | Runs | OK | Mean TPS | Mean Duration (ms) |\n|---|---|---|---|---|---|\n"
        rows   (for [{:keys [model prompt summary]} results
                     :let [{:keys [runs_total runs_ok mean_tps mean_duration_ms]} summary]]
                 (format "| `%s` | %s | %d | %d | %.2f | %.1f |\n"
                         model
                         (str/replace prompt #"\n" " ")
                         runs_total
                         runs_ok
                         (or mean_tps 0.0)
                         (or mean_duration_ms 0.0)))]
    (str "# Ollama benchmark\n\n"
         header
         (apply str rows)
         "\n")))