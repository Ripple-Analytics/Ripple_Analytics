(ns mental-models.main
  "Mental Models System - Electric Clojure
   Unified frontend/backend with automatic real-time updates"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            #?(:clj [mental-models.services.llm :as llm])
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

;; -- Server Entry Point ------------------------------------------------------

#?(:clj
   (defn -main [& args]
     (println "Starting Mental Models Electric Server...")
     (require '[mental-models.server :as server])
     ((resolve 'mental-models.server/start!))))
