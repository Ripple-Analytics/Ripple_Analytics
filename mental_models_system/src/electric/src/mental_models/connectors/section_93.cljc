(ns mental-models.connectors.section-93
  "Connectors Module - Section 93"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def metrics-export-config
  "Configuration for metrics export."
  (atom {:enabled true
         :format :prometheus
         :include-labels true
         :prefix "connector_"}))

(defn get-metrics-export-config
  "Get current metrics export configuration."
  []
  @metrics-export-config)

(defn set-metrics-export-config
  "Update metrics export configuration."
  [config]
  (swap! metrics-export-config merge config))

(defn format-prometheus-metric
  "Format a metric in Prometheus format."
  [name value & {:keys [labels help type]}]
  (let [config @metrics-export-config
        prefix (:prefix config)
        label-str (when (and (:include-labels config) labels)
                    (str "{"
                         (clojure.string/join ","
                                              (map (fn [[k v]] (str (name k) "=\"" v "\"")) labels))
                         "}"))]
    (str (when help (str "# HELP " prefix name " " help "\n"))
         (when type (str "# TYPE " prefix name " " type "\n"))
         prefix name (or label-str "") " " value)))

(defn export-metrics-prometheus
  "Export all connector metrics in Prometheus format."
  []
  (let [metrics-data @metrics
        lines (atom [])]
    ;; Request counts
    (doseq [[connector data] metrics-data]
      (swap! lines conj
             (format-prometheus-metric "requests_total"
                                       (:total-requests data 0)
                                       :labels {:connector (name connector)}
                                       :type "counter")))
    ;; Error counts
    (doseq [[connector data] metrics-data]
      (swap! lines conj
             (format-prometheus-metric "errors_total"
                                       (:total-errors data 0)
                                       :labels {:connector (name connector)}
                                       :type "counter")))
    ;; Latency
    (doseq [[connector data] metrics-data]
      (swap! lines conj
             (format-prometheus-metric "latency_ms"
                                       (:avg-latency-ms data 0)
                                       :labels {:connector (name connector)}
                                       :type "gauge")))
    (clojure.string/join "\n" @lines)))

(defn export-metrics-json
  "Export all connector metrics in JSON format."
  []
  {:timestamp (System/currentTimeMillis)
   :metrics @metrics
   :health-scores @health-scores
   :circuit-states (into {}
                         (map (fn [[k v]] [k {:state (:state v) :failures (:failures v)}])
                              @circuit-breakers))
   :throttle-stats (get-throttle-stats)
   :cache-stats {:size (count @response-cache)}})

(defn export-metrics
  "Export metrics in the configured format."
  []
  (let [config @metrics-export-config]
    (if-not (:enabled config)
      {:error "Metrics export disabled"}
      (case (:format config)
        :prometheus (export-metrics-prometheus)
        :json (export-metrics-json)
        (export-metrics-json)))))

(defn get-metrics-export-stats
  "Get statistics about metrics export."
  []
  {:config @metrics-export-config
   :available-formats [:prometheus :json]
   :metrics-count (count @metrics)})

(def connector-registry
  "Registry of all available connectors."
  {:zapier {:create create-zapier-connector
            :description "Zapier webhook integration"}
   :huggingface {:create create-huggingface-connector
                 :description "Huggingface AI models"}
   :github {:create create-github-connector
            :description "GitHub API integration"}
   :slack {:create create-slack-connector
           :description "Slack messaging integration"}
   :google-drive {:create create-google-drive-connector
                  :description "Google Drive file access"}
   :lm-studio {:create create-lm-studio-connector
               :description "Local LLM via LM Studio"}
   :database {:create create-database-connector
              :description "Database connectivity"}
   :file {:create create-file-connector
          :description "File system access"}
   :web-scraper {:create create-web-scraper
                 :description "Web scraping"}})

(defn list-connectors
  "List all available connectors."
  []
  (mapv (fn [[k v]]
          {:type k
           :description (:description v)})
        connector-registry))

(defn create-connector
  "Create a connector by type."
  [connector-type & args]
  (if-let [connector-def (get connector-registry connector-type)]
    (apply (:create connector-def) args)
    {:error (str "Unknown connector type: " connector-type)}))

