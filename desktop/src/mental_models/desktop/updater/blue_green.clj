(ns mental-models.desktop.updater.blue-green
  "Blue-Green Deployment Manager
   
   Maintains two deployment slots (blue/green) for zero-downtime updates:
   - One slot is always 'active' serving requests
   - Updates deploy to the 'inactive' slot
   - Health checks verify the new version
   - Traffic switches atomically to new version
   - Old version kept for instant rollback
   
   This enables continuous operation even during updates."
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.time Instant Duration]
           [java.nio.file Files Paths StandardCopyOption]
           [java.util.concurrent.atomic AtomicReference]))

;; =============================================================================
;; Deployment Slots
;; =============================================================================

(def base-dir (str (System/getProperty "user.home") "/.mental-models"))

(def slots
  {:blue {:id :blue
          :dir (str base-dir "/deployments/blue")
          :port 8081}
   :green {:id :green
           :dir (str base-dir "/deployments/green")
           :port 8082}})

(defonce deployment-state
  (atom {:active-slot :blue
         :inactive-slot :green
         :blue {:version nil
                :status :stopped      ;; :stopped, :starting, :running, :stopping, :failed
                :health :unknown      ;; :unknown, :healthy, :unhealthy
                :started-at nil
                :process nil}
         :green {:version nil
                 :status :stopped
                 :health :unknown
                 :started-at nil
                 :process nil}
         :last-deployment nil
         :deployment-history []
         :rollback-available false}))

;; =============================================================================
;; Slot Management
;; =============================================================================

(defn get-active-slot []
  "Get the currently active deployment slot"
  (get slots (:active-slot @deployment-state)))

(defn get-inactive-slot []
  "Get the inactive deployment slot (for staging updates)"
  (get slots (:inactive-slot @deployment-state)))

(defn get-slot-status [slot-id]
  "Get the status of a specific slot"
  (get @deployment-state slot-id))

(defn set-slot-status! [slot-id status]
  "Update the status of a slot"
  (swap! deployment-state assoc-in [slot-id :status] status)
  (log/info "Slot" slot-id "status changed to:" status))

(defn set-slot-health! [slot-id health]
  "Update the health of a slot"
  (swap! deployment-state assoc-in [slot-id :health] health))

(defn set-slot-version! [slot-id version]
  "Update the version deployed to a slot"
  (swap! deployment-state assoc-in [slot-id :version] version))

;; =============================================================================
;; Health Checks
;; =============================================================================

(def health-check-config
  {:interval-ms 5000          ;; Check every 5 seconds
   :timeout-ms 3000           ;; 3 second timeout
   :healthy-threshold 3       ;; 3 consecutive successes = healthy
   :unhealthy-threshold 2})   ;; 2 consecutive failures = unhealthy

(defn perform-health-check [slot]
  "Perform a health check on a deployment slot"
  (try
    ;; Check if the process is running and responsive
    (let [process (get-in @deployment-state [(:id slot) :process])]
      (if (and process (.isAlive process))
        (do
          ;; Could add HTTP health endpoint check here
          ;; For now, just check process is alive
          {:healthy true :latency-ms 0})
        {:healthy false :error "Process not running"}))
    (catch Exception e
      {:healthy false :error (.getMessage e)})))

(defn start-health-monitor! [slot-id]
  "Start continuous health monitoring for a slot"
  (go-loop [consecutive-healthy 0
            consecutive-unhealthy 0]
    (<! (timeout (:interval-ms health-check-config)))
    (let [slot (get slots slot-id)
          result (perform-health-check slot)]
      (if (:healthy result)
        (let [new-healthy (inc consecutive-healthy)]
          (when (>= new-healthy (:healthy-threshold health-check-config))
            (set-slot-health! slot-id :healthy))
          (recur new-healthy 0))
        (let [new-unhealthy (inc consecutive-unhealthy)]
          (when (>= new-unhealthy (:unhealthy-threshold health-check-config))
            (set-slot-health! slot-id :unhealthy)
            (log/warn "Slot" slot-id "is unhealthy:" (:error result)))
          (recur 0 new-unhealthy))))))

;; =============================================================================
;; Deployment Operations
;; =============================================================================

(defn prepare-slot! [slot version-sha source-dir]
  "Prepare a slot with new code"
  (log/info "Preparing slot" (:id slot) "with version" version-sha)
  (let [target-dir (io/file (:dir slot))]
    ;; Clean the target directory
    (when (.exists target-dir)
      (log/debug "Cleaning existing deployment at" (.getAbsolutePath target-dir))
      (doseq [f (reverse (file-seq target-dir))]
        (.delete f)))
    
    ;; Copy new code
    (.mkdirs target-dir)
    (log/debug "Copying from" source-dir "to" (.getAbsolutePath target-dir))
    
    ;; Copy all files from source to target
    (doseq [f (file-seq (io/file source-dir))
            :when (.isFile f)]
      (let [relative-path (.relativize (.toPath (io/file source-dir)) (.toPath f))
            target-file (io/file target-dir (.toString relative-path))]
        (io/make-parents target-file)
        (io/copy f target-file)))
    
    ;; Update slot version
    (set-slot-version! (:id slot) version-sha)
    (log/info "Slot" (:id slot) "prepared with version" version-sha)
    {:success true}))

(defn start-slot! [slot]
  "Start the application in a deployment slot"
  (log/info "Starting slot" (:id slot))
  (set-slot-status! (:id slot) :starting)
  
  (try
    (let [working-dir (io/file (:dir slot))
          ;; Start Clojure process with the slot's code
          pb (ProcessBuilder. 
               ["clojure" "-M:run" "start" 
                "--port" (str (:port slot))])
          _ (.directory pb working-dir)
          _ (.inheritIO pb)
          process (.start pb)]
      
      ;; Store process reference
      (swap! deployment-state assoc-in [(:id slot) :process] process)
      (swap! deployment-state assoc-in [(:id slot) :started-at] (str (Instant/now)))
      
      ;; Wait for startup
      (Thread/sleep 5000)
      
      (if (.isAlive process)
        (do
          (set-slot-status! (:id slot) :running)
          (start-health-monitor! (:id slot))
          (log/info "Slot" (:id slot) "started successfully")
          {:success true})
        (do
          (set-slot-status! (:id slot) :failed)
          (log/error "Slot" (:id slot) "failed to start")
          {:success false :error "Process exited"})))
    (catch Exception e
      (set-slot-status! (:id slot) :failed)
      (log/error e "Failed to start slot" (:id slot))
      {:success false :error (.getMessage e)})))

(defn stop-slot! [slot]
  "Stop the application in a deployment slot"
  (log/info "Stopping slot" (:id slot))
  (set-slot-status! (:id slot) :stopping)
  
  (when-let [process (get-in @deployment-state [(:id slot) :process])]
    (try
      ;; Send graceful shutdown signal
      (.destroy process)
      
      ;; Wait for graceful shutdown (max 30 seconds)
      (let [exited (.waitFor process 30 java.util.concurrent.TimeUnit/SECONDS)]
        (when-not exited
          (log/warn "Slot" (:id slot) "did not stop gracefully, forcing...")
          (.destroyForcibly process)))
      
      (swap! deployment-state assoc-in [(:id slot) :process] nil)
      (set-slot-status! (:id slot) :stopped)
      (log/info "Slot" (:id slot) "stopped")
      {:success true}
      (catch Exception e
        (log/error e "Error stopping slot" (:id slot))
        {:success false :error (.getMessage e)}))))

;; =============================================================================
;; Traffic Switching
;; =============================================================================

(defn switch-traffic! []
  "Atomically switch traffic from active to inactive slot"
  (let [current-active (:active-slot @deployment-state)
        current-inactive (:inactive-slot @deployment-state)
        inactive-health (get-in @deployment-state [current-inactive :health])]
    
    (if (= inactive-health :healthy)
      (do
        (log/info "🔄 Switching traffic from" current-active "to" current-inactive)
        (swap! deployment-state assoc 
               :active-slot current-inactive
               :inactive-slot current-active
               :rollback-available true)
        
        ;; Record in history
        (swap! deployment-state update :deployment-history conj
               {:timestamp (str (Instant/now))
                :from current-active
                :to current-inactive
                :version (get-in @deployment-state [current-inactive :version])})
        
        (log/info "✅ Traffic switched to" current-inactive)
        {:success true :new-active current-inactive})
      (do
        (log/error "Cannot switch traffic - inactive slot is not healthy:" inactive-health)
        {:success false :error "Inactive slot not healthy"}))))

(defn rollback! []
  "Rollback to the previous deployment"
  (if (:rollback-available @deployment-state)
    (do
      (log/warn "🔙 Rolling back deployment...")
      (switch-traffic!)
      (swap! deployment-state assoc :rollback-available false)
      (log/info "Rollback complete"))
    (log/warn "No rollback available")))

;; =============================================================================
;; Full Deployment Flow
;; =============================================================================

(defn deploy! [version-sha source-dir]
  "Execute a full blue-green deployment
   
   Steps:
   1. Prepare inactive slot with new code
   2. Start the new version
   3. Wait for health checks to pass
   4. Switch traffic to new version
   5. Keep old version running for rollback"
  (log/info "========================================")
  (log/info "Starting deployment of version:" version-sha)
  (log/info "========================================")
  
  (let [inactive (get-inactive-slot)
        active (get-active-slot)]
    
    ;; Step 1: Prepare inactive slot
    (log/info "Step 1/4: Preparing inactive slot" (:id inactive))
    (let [prep-result (prepare-slot! inactive version-sha source-dir)]
      (when-not (:success prep-result)
        (log/error "Deployment failed at preparation")
        (throw (ex-info "Preparation failed" prep-result))))
    
    ;; Step 2: Start new version
    (log/info "Step 2/4: Starting new version")
    (let [start-result (start-slot! inactive)]
      (when-not (:success start-result)
        (log/error "Deployment failed at startup")
        (throw (ex-info "Startup failed" start-result))))
    
    ;; Step 3: Wait for health
    (log/info "Step 3/4: Waiting for health checks...")
    (loop [attempts 0]
      (Thread/sleep 5000)
      (let [health (get-in @deployment-state [(:id inactive) :health])]
        (cond
          (= health :healthy)
          (log/info "Health checks passed!")
          
          (>= attempts 12)  ;; 60 seconds max
          (do
            (log/error "Health checks timed out")
            (stop-slot! inactive)
            (throw (ex-info "Health check timeout" {:attempts attempts})))
          
          :else
          (do
            (log/debug "Waiting for health... attempt" (inc attempts))
            (recur (inc attempts))))))
    
    ;; Step 4: Switch traffic
    (log/info "Step 4/4: Switching traffic")
    (let [switch-result (switch-traffic!)]
      (when-not (:success switch-result)
        (log/error "Traffic switch failed")
        (stop-slot! inactive)
        (throw (ex-info "Traffic switch failed" switch-result))))
    
    ;; Success!
    (swap! deployment-state assoc :last-deployment (str (Instant/now)))
    (log/info "========================================")
    (log/info "✅ Deployment complete!")
    (log/info "   Active slot:" (:active-slot @deployment-state))
    (log/info "   Version:" version-sha)
    (log/info "========================================")
    
    {:success true
     :active-slot (:active-slot @deployment-state)
     :version version-sha}))

;; =============================================================================
;; Status & Monitoring
;; =============================================================================

(defn get-deployment-status []
  "Get full deployment status"
  (let [state @deployment-state
        active-slot (get-active-slot)
        inactive-slot (get-inactive-slot)]
    {:active {:slot (:id active-slot)
              :version (get-in state [(:id active-slot) :version])
              :status (get-in state [(:id active-slot) :status])
              :health (get-in state [(:id active-slot) :health])
              :port (:port active-slot)}
     :inactive {:slot (:id inactive-slot)
                :version (get-in state [(:id inactive-slot) :version])
                :status (get-in state [(:id inactive-slot) :status])
                :health (get-in state [(:id inactive-slot) :health])
                :port (:port inactive-slot)}
     :last-deployment (:last-deployment state)
     :rollback-available (:rollback-available state)
     :history (take 10 (:deployment-history state))}))

(defn print-status []
  "Print deployment status to console"
  (let [status (get-deployment-status)]
    (println "\n╔════════════════════════════════════════╗")
    (println "║       DEPLOYMENT STATUS                ║")
    (println "╠════════════════════════════════════════╣")
    (println (str "║ Active:   " (name (get-in status [:active :slot])) 
                  " (port " (get-in status [:active :port]) ")"))
    (println (str "║ Version:  " (or (get-in status [:active :version]) "none")))
    (println (str "║ Status:   " (name (get-in status [:active :status]))))
    (println (str "║ Health:   " (name (get-in status [:active :health]))))
    (println "╠════════════════════════════════════════╣")
    (println (str "║ Standby:  " (name (get-in status [:inactive :slot]))))
    (println (str "║ Version:  " (or (get-in status [:inactive :version]) "none")))
    (println (str "║ Rollback: " (if (:rollback-available status) "available" "none")))
    (println "╚════════════════════════════════════════╝\n")))
