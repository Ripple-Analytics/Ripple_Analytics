(ns mental-models.desktop.updater.continuous-deploy
  "Continuous Deployment Pipeline - Orchestrates all update components
   
   Integrates:
   - GitHub checker (5-minute polling)
   - Blue-green deployment (zero downtime)
   - Hot code reloading (sub-second updates)
   - Graceful job handoff (no work lost)
   
   The pipeline automatically:
   1. Checks GitHub every 5 minutes
   2. Downloads new code when available
   3. Deploys using blue-green strategy
   4. Hot-reloads when possible, full deploy when needed
   5. Hands off jobs gracefully between versions"
  (:require [mental-models.desktop.updater.github-checker :as github]
            [mental-models.desktop.updater.blue-green :as bg]
            [mental-models.desktop.updater.hot-reload :as hot]
            [mental-models.desktop.updater.job-handoff :as handoff]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout alts!]]
            [clojure.java.io :as io]
            [taoensso.timbre :as log])
  (:import [java.time Instant]
           [java.io File]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def pipeline-config
  (atom {:auto-deploy true              ;; Automatically deploy updates
         :hot-reload-enabled true       ;; Try hot reload before full deploy
         :require-approval false        ;; Require user approval for deploys
         :notify-on-update true         ;; Send notifications on update
         :max-deploy-attempts 3         ;; Max retry attempts
         :deploy-cooldown-ms (* 5 60 1000)  ;; 5 min between deploys
         :source-dirs ["src" "desktop/src"]}))

(defonce pipeline-state
  (atom {:status :idle                  ;; :idle, :checking, :downloading, :deploying, :error
         :last-deploy nil
         :deploy-count 0
         :error-count 0
         :last-error nil
         :pending-approval nil
         :deployment-log []}))

(defonce control-channel (chan 100))
(defonce pipeline-running (atom false))

;; =============================================================================
;; Deployment Decision Logic
;; =============================================================================

(defn can-hot-reload?
  "Determine if changes can be hot-reloaded vs requiring full deploy"
  [changed-files]
  (let [clj-only (every? #(or (.endsWith % ".clj") 
                               (.endsWith % ".cljc")
                               (.endsWith % ".edn")) 
                         changed-files)
        no-deps-change (not-any? #(or (.contains % "deps.edn")
                                       (.contains % "project.clj")
                                       (.contains % "pom.xml"))
                                  changed-files)
        no-java-change (not-any? #(.endsWith % ".java") changed-files)]
    (and clj-only no-deps-change no-java-change)))

(defn should-deploy?
  "Determine if we should proceed with deployment"
  []
  (let [state @pipeline-state
        config @pipeline-config
        now (System/currentTimeMillis)
        last-deploy (:last-deploy state)
        cooldown-ok (or (nil? last-deploy)
                        (> (- now last-deploy) (:deploy-cooldown-ms config)))]
    (and (:auto-deploy config)
         cooldown-ok
         (not= (:status state) :deploying))))

;; =============================================================================
;; Deployment Strategies
;; =============================================================================

(defn deploy-hot-reload!
  "Deploy using hot code reloading (fastest, for Clojure-only changes)"
  [changed-files]
  (log/info "🔥 Attempting hot reload deployment...")
  (swap! pipeline-state assoc :status :deploying)
  
  (try
    (let [result (hot/reload-changed!)]
      (if (:success result)
        (do
          (log/info "✅ Hot reload successful:" (count (:reloaded result)) "namespaces")
          (swap! pipeline-state assoc 
                 :status :idle
                 :last-deploy (System/currentTimeMillis)
                 :deploy-count (inc (:deploy-count @pipeline-state)))
          (swap! pipeline-state update :deployment-log conj
                 {:type :hot-reload
                  :timestamp (str (Instant/now))
                  :namespaces (:reloaded result)
                  :success true})
          {:success true :type :hot-reload})
        (do
          (log/warn "Hot reload failed, falling back to blue-green")
          {:success false :fallback true})))
    (catch Exception e
      (log/error e "Hot reload error")
      {:success false :fallback true :error (.getMessage e)})))

(defn deploy-blue-green!
  "Deploy using blue-green strategy (full deploy, zero downtime)"
  [version-sha source-dir]
  (log/info "🔵🟢 Starting blue-green deployment...")
  (swap! pipeline-state assoc :status :deploying)
  
  (try
    ;; Step 1: Prepare job handoff
    (log/info "Step 1: Preparing job handoff...")
    (let [handoff-result (handoff/prepare-handoff!)]
      (when-not (:success handoff-result)
        (throw (ex-info "Job handoff failed" handoff-result)))
      
      ;; Step 2: Deploy to inactive slot
      (log/info "Step 2: Deploying to inactive slot...")
      (let [deploy-result (bg/deploy! version-sha source-dir)]
        (when-not (:success deploy-result)
          (handoff/abort-handoff!)
          (throw (ex-info "Blue-green deploy failed" deploy-result)))
        
        ;; Step 3: Transfer pending jobs
        (log/info "Step 3: Transferring pending jobs...")
        (when-let [pending (:pending-jobs handoff-result)]
          (handoff/import-pending-jobs! pending))
        
        ;; Step 4: Complete handoff
        (log/info "Step 4: Completing handoff...")
        (handoff/complete-handoff!)
        
        ;; Update state
        (swap! pipeline-state assoc 
               :status :idle
               :last-deploy (System/currentTimeMillis)
               :deploy-count (inc (:deploy-count @pipeline-state)))
        (swap! pipeline-state update :deployment-log conj
               {:type :blue-green
                :timestamp (str (Instant/now))
                :version version-sha
                :success true})
        
        ;; Update local version
        (github/write-local-version version-sha)
        
        (log/info "✅ Blue-green deployment complete!")
        {:success true :type :blue-green :version version-sha})))
    
    (catch Exception e
      (log/error e "Blue-green deployment failed")
      (swap! pipeline-state assoc 
             :status :error
             :last-error (.getMessage e)
             :error-count (inc (:error-count @pipeline-state)))
      (swap! pipeline-state update :deployment-log conj
             {:type :blue-green
              :timestamp (str (Instant/now))
              :success false
              :error (.getMessage e)})
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Update Handler
;; =============================================================================

(defn handle-update-available!
  "Handle notification that an update is available"
  [update-info]
  (log/info "========================================")
  (log/info "📦 Update available!")
  (log/info "   Current:" (:current update-info))
  (log/info "   Latest:" (get-in update-info [:latest :sha]))
  (log/info "   Message:" (get-in update-info [:latest :message]))
  (log/info "========================================")
  
  (if-not (should-deploy?)
    (do
      (log/info "Skipping deployment (cooldown or disabled)")
      {:deployed false :reason "cooldown"})
    
    (if (:require-approval @pipeline-config)
      (do
        (log/info "Waiting for approval...")
        (swap! pipeline-state assoc :pending-approval update-info)
        {:deployed false :reason "awaiting-approval"})
      
      ;; Auto-deploy
      (do
        (log/info "Starting automatic deployment...")
        (swap! pipeline-state assoc :status :downloading)
        
        ;; Download the update
        (let [download-result (github/download-archive (get-in update-info [:latest :sha]))]
          (if-not (:success download-result)
            (do
              (log/error "Download failed:" (:error download-result))
              (swap! pipeline-state assoc :status :error :last-error (:error download-result))
              {:deployed false :error (:error download-result)})
            
            ;; Extract and check what changed
            (let [staging-dir (str (System/getProperty "user.home") "/.mental-models/staging")
                  archive-file (:file download-result)
                  extract-result (github/extract-archive archive-file (io/file staging-dir))]
              
              (if-not (:success extract-result)
                (do
                  (log/error "Extract failed:" (:error extract-result))
                  {:deployed false :error (:error extract-result)})
                
                ;; Determine deployment strategy
                (let [diff (github/get-commit-diff (:current update-info) 
                                                    (get-in update-info [:latest :sha]))
                      changed-files (map :filename (:files diff))
                      can-hot-reload (and (:hot-reload-enabled @pipeline-config)
                                          (can-hot-reload? changed-files))]
                  
                  (log/info "Changed files:" (count changed-files))
                  (log/info "Can hot reload:" can-hot-reload)
                  
                  (if can-hot-reload
                    ;; Try hot reload first
                    (let [result (deploy-hot-reload! changed-files)]
                      (if (and (not (:success result)) (:fallback result))
                        ;; Fall back to blue-green
                        (deploy-blue-green! (get-in update-info [:latest :sha]) staging-dir)
                        result))
                    
                    ;; Full blue-green deploy
                    (deploy-blue-green! (get-in update-info [:latest :sha]) staging-dir)))))))))))

;; =============================================================================
;; Pipeline Control
;; =============================================================================

(defn start-pipeline!
  "Start the continuous deployment pipeline"
  []
  (when @pipeline-running
    (log/warn "Pipeline already running")
    (throw (ex-info "Pipeline already running" {})))
  
  (log/info "╔════════════════════════════════════════════════════════════╗")
  (log/info "║     CONTINUOUS DEPLOYMENT PIPELINE STARTING                ║")
  (log/info "╚════════════════════════════════════════════════════════════╝")
  
  (reset! pipeline-running true)
  
  ;; Start GitHub checker
  (github/start-update-checker!)
  
  ;; Start hot reload watcher
  (when (:hot-reload-enabled @pipeline-config)
    (hot/start-auto-reload! (:source-dirs @pipeline-config)))
  
  ;; Start update handler loop
  (go-loop []
    (when @pipeline-running
      (let [[msg ch] (alts! [github/update-channel control-channel (timeout 60000)])]
        (cond
          (= ch control-channel)
          (case (:command msg)
            :stop (log/info "Pipeline stopping...")
            :force-check (github/force-check!)
            :force-deploy (when-let [update (:update msg)]
                            (handle-update-available! update))
            (log/warn "Unknown command:" msg))
          
          (and msg (= (:type msg) :update-available))
          (handle-update-available! msg)
          
          :else nil)
        (recur))))
  
  (log/info "Pipeline started - checking for updates every 5 minutes")
  {:success true})

(defn stop-pipeline!
  "Stop the continuous deployment pipeline"
  []
  (log/info "Stopping continuous deployment pipeline...")
  (reset! pipeline-running false)
  (async/put! control-channel {:command :stop})
  (github/stop-update-checker!)
  (hot/stop-auto-reload!)
  (log/info "Pipeline stopped")
  {:success true})

(defn force-check!
  "Force an immediate update check"
  []
  (async/put! control-channel {:command :force-check}))

(defn approve-pending!
  "Approve a pending deployment"
  []
  (when-let [pending (:pending-approval @pipeline-state)]
    (swap! pipeline-state dissoc :pending-approval)
    (handle-update-available! pending)))

(defn reject-pending!
  "Reject a pending deployment"
  []
  (when (:pending-approval @pipeline-state)
    (log/info "Deployment rejected")
    (swap! pipeline-state dissoc :pending-approval)))

;; =============================================================================
;; Status & Configuration
;; =============================================================================

(defn get-pipeline-status []
  "Get comprehensive pipeline status"
  {:pipeline @pipeline-state
   :github (github/get-update-status)
   :deployment (bg/get-deployment-status)
   :hot-reload (hot/get-reload-status)
   :handoff (handoff/get-handoff-status)
   :config @pipeline-config})

(defn print-status []
  "Print full pipeline status"
  (let [status (get-pipeline-status)]
    (println "\n╔════════════════════════════════════════════════════════════╗")
    (println "║         CONTINUOUS DEPLOYMENT PIPELINE STATUS              ║")
    (println "╠════════════════════════════════════════════════════════════╣")
    (println (str "║ Status: " (name (get-in status [:pipeline :status]))))
    (println (str "║ Running: " @pipeline-running))
    (println (str "║ Deploys: " (get-in status [:pipeline :deploy-count])))
    (println (str "║ Errors: " (get-in status [:pipeline :error-count])))
    (println "╠════════════════════════════════════════════════════════════╣")
    (println (str "║ Current version: " (get-in status [:github :current-sha])))
    (println (str "║ Latest version: " (get-in status [:github :latest-sha])))
    (println (str "║ Update available: " (get-in status [:github :update-available])))
    (println (str "║ Last check: " (get-in status [:github :last-check])))
    (println "╠════════════════════════════════════════════════════════════╣")
    (println (str "║ Active slot: " (get-in status [:deployment :active :slot])))
    (println (str "║ Hot reloads: " (get-in status [:hot-reload :reload-count])))
    (println (str "║ Running jobs: " (get-in status [:handoff :running-jobs])))
    (println "╚════════════════════════════════════════════════════════════╝\n")))

(defn configure!
  "Update pipeline configuration"
  [config-map]
  (swap! pipeline-config merge config-map)
  (log/info "Pipeline configuration updated:" config-map))

;; =============================================================================
;; CLI Commands
;; =============================================================================

(defn -main
  "CLI entry point for continuous deployment"
  [& args]
  (let [cmd (first args)]
    (case cmd
      "start" (start-pipeline!)
      "stop" (stop-pipeline!)
      "status" (print-status)
      "check" (force-check!)
      "approve" (approve-pending!)
      "reject" (reject-pending!)
      "rollback" (bg/rollback!)
      (do
        (println "Usage: continuous-deploy <command>")
        (println "Commands:")
        (println "  start    - Start the continuous deployment pipeline")
        (println "  stop     - Stop the pipeline")
        (println "  status   - Show pipeline status")
        (println "  check    - Force an update check")
        (println "  approve  - Approve pending deployment")
        (println "  reject   - Reject pending deployment")
        (println "  rollback - Rollback to previous version")))))
