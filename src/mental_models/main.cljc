(ns mental-models.main
  "Mental Models System - Electric Clojure
   Unified frontend/backend with automatic real-time updates
   Integrated analysis engines: Bayesian, HMM, Statistical, Decision Tracking"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            #?(:clj [mental-models.db.analysis :as db-analysis])
            #?(:clj [mental-models.services.llm :as llm])
            #?(:clj [mental-models.services.analyzer :as analyzer])
            #?(:clj [mental-models.analysis.core :as analysis-core])
            #?(:clj [mental-models.analysis.bayesian :as bayesian])
            #?(:clj [mental-models.analysis.statistical :as stats])
            #?(:clj [mental-models.analysis.hmm :as hmm])
            #?(:clj [mental-models.analysis.decision-engine :as decision-engine])
            #?(:clj [mental-models.io.file-ingestion :as file-ingestion])
            [mental-models.ui.components :as ui]
            [mental-models.ui.dashboard :as dashboard]
            [mental-models.ui.models :as models]
            [mental-models.ui.analysis :as analysis]
            [mental-models.ui.decisions :as decisions]
            [mental-models.ui.metrics :as metrics]
            [mental-models.ui.world-map :as world-map]
            [mental-models.ui.knowledge-graph :as knowledge-graph]
            [mental-models.ui.signals :as signals]
            [mental-models.ui.connectors :as connectors]))

;; -- Routing State -----------------------------------------------------------

(e/def current-route (e/client (atom {:page :dashboard :params {}})))

(e/defn navigate! [page & [params]]
  (e/client (reset! current-route {:page page :params (or params {})})))

;; -- Main Application --------------------------------------------------------

(e/defn App []
  (e/client
    (dom/div
      (dom/props {:class "min-h-screen bg-zinc-950 text-zinc-100"})
      
      ;; Sidebar Navigation
      (ui/Sidebar. current-route navigate!)
      
      ;; Main Content Area
      (dom/main
        (dom/props {:class "ml-64 p-8"})
        
        (let [{:keys [page params]} @current-route]
          (case page
            :dashboard (dashboard/Dashboard.)
            :models (models/ModelsExplorer.)
            :model-detail (models/ModelDetail. (:id params))
            :analysis (analysis/AnalysisPage.)
            :decisions (decisions/DecisionJournal.)
            :effectiveness (analysis/EffectivenessTracker.)
            :lollapalooza (analysis/LollapaloozaDetector.)
            :failure-modes (models/FailureModes.)
            :world-map (world-map/WorldMap.)
            :knowledge-graph (knowledge-graph/KnowledgeGraph.)
            :signals (signals/SignalHarvester.)
            :connectors (connectors/ConnectorHub.)
            :metrics (metrics/MetricsDashboard.)
            :statistics (metrics/StatisticsPage.)
            :documents (analysis/DocumentAnalysis.)
            
            ;; Default
            (dashboard/Dashboard.)))))))

;; -- Analysis State (Server-side) -----------------------------------------------

#?(:clj
   (do
     (def analysis-cache (atom {}))
     (def decision-log (atom []))
     (def folder-watchers (atom {}))
     
     ;; Analyze text with all engines
     (defn analyze-text! [text user-id]
       (let [result (analysis-core/analyze-document
                     {:document-id (java.util.UUID/randomUUID)
                      :document-path "user-input"
                      :content text
                      :metadata {:user-id user-id}})
             stored (db-analysis/save-analysis!
                    {:id (:id result)
                     :user-id user-id
                     :text text
                     :text-length (count text)
                     :source "manual"
                     :models-analyzed (:models-analyzed result)
                     :successful-analyses (count (filter :success (:all-scores result)))
                     :average-score (:average-score result)
                     :lollapalooza-detected (:lollapalooza-detected result)
                     :convergence-score (:convergence-score result)
                     :convergence-count (:convergence-count result)
                     :top-10-models (:applicable-models result)
                     :all-scores (:all-scores result)
                     :created-at (:analyzed-at result)})
             ]
         (swap! analysis-cache assoc (:id result) result)
         result))
     
     ;; Watch folder for new files
     (defn watch-folder! [folder-path user-id]
       (let [watch-chan (file-ingestion/watch-folder folder-path)]
         (future
           (loop []
             (when-let [filepath (async/<!! watch-chan)]
               (try
                 (let [text (file-ingestion/extract-text filepath)
                       metadata (file-ingestion/get-file-metadata filepath)]
                   (when (file-ingestion/validate-text text)
                     (analyze-text! text user-id)))
                 (catch Exception e
                   (log/error e "Error processing file" filepath)))
               (recur))))
         (swap! folder-watchers assoc folder-path watch-chan)))
     
     ;; Track decision
     (defn track-decision! [decision-text context models confidence user-id]
       (let [decision (decision-engine/create-decision decision-text context models confidence)]
         (swap! decision-log conj decision)
         decision))
     
     ;; Record outcome
     (defn record-outcome! [decision-id outcome-text score actual-vs-predicted user-id]
       (let [outcome (decision-engine/record-outcome decision-id outcome-text score actual-vs-predicted)]
         (swap! decision-log conj outcome)
         outcome))))

;; -- Server Entry Point ------------------------------------------------------

#?(:clj
   (defn -main [& args]
     (println "Starting Mental Models Electric Server...")
     (println "Integrated Analysis Engines:")
     (println "  ✓ Bayesian Inference")
     (println "  ✓ Hidden Markov Models")
     (println "  ✓ Statistical Analysis")
     (println "  ✓ Decision Tracking")
     (println "  ✓ File Ingestion & Folder Watching")
     (require '[mental-models.server :as server])
     ((resolve 'mental-models.server/start!))))
