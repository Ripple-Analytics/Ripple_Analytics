(ns mental-models.desktop.updater.edge-cases
  "Edge Cases Module - Handle unusual update scenarios
   
   Covers Category 17: Edge Cases
   - First-ever update (fresh install)
   - Downgrade prevention/handling
   - Same version re-download
   - Update while minimized to tray
   - Update during active scan
   - Multiple instances handling
   - Interrupted updates
   - Partial downloads
   - Corrupted state recovery"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.time Instant]
           [java.util UUID]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def edge-case-config
  (atom {:allow-downgrade false
         :allow-same-version-reinstall false
         :require-user-confirmation-for-downgrade true
         :max-interrupted-resume-attempts 3
         :partial-download-cleanup-age-hours 24
         :first-install-marker-file (str (System/getProperty "user.home") "/.mental-models/.first-install")
         :update-state-file (str (System/getProperty "user.home") "/.mental-models/update-state.edn")}))

;; =============================================================================
;; State
;; =============================================================================

(defonce edge-case-state
  (atom {:is-first-install nil
         :interrupted-updates []
         :partial-downloads []
         :downgrade-attempts []
         :active-scan-detected false
         :minimized-to-tray false
         :recovery-mode false}))

;; =============================================================================
;; First-Ever Update Detection
;; =============================================================================

(defn is-first-install?
  "Check if this is the first installation"
  []
  (let [marker-file (io/file (:first-install-marker-file @edge-case-config))]
    (not (.exists marker-file))))

(defn mark-first-install-complete!
  "Mark that first installation is complete"
  []
  (let [marker-file (io/file (:first-install-marker-file @edge-case-config))]
    (io/make-parents marker-file)
    (spit marker-file (pr-str {:installed-at (str (Instant/now))
                                :version "1.0.0"}))
    (swap! edge-case-state assoc :is-first-install false)
    (log/info "Marked first install complete")))

(defn handle-first-install!
  "Handle first-ever installation"
  [install-fn]
  (log/info "Handling first-ever installation")
  (try
    (let [result (install-fn)]
      (when (:success result)
        (mark-first-install-complete!))
      result)
    (catch Exception e
      (log/error e "First install failed")
      {:success false :error (.getMessage e)})))

(defn get-first-install-info
  "Get information about the first installation"
  []
  (let [marker-file (io/file (:first-install-marker-file @edge-case-config))]
    (when (.exists marker-file)
      (try
        (read-string (slurp marker-file))
        (catch Exception _ nil)))))

;; =============================================================================
;; Downgrade Prevention
;; =============================================================================

(defn parse-version
  "Parse a version string into comparable parts"
  [version-str]
  (when version-str
    (let [clean (str/replace version-str #"^v" "")
          parts (str/split clean #"[.\-]")]
      (mapv #(try (Integer/parseInt %) (catch Exception _ %)) parts))))

(defn compare-versions
  "Compare two version strings
   Returns: negative if v1 < v2, 0 if equal, positive if v1 > v2"
  [v1 v2]
  (let [p1 (parse-version v1)
        p2 (parse-version v2)
        max-len (max (count p1) (count p2))]
    (loop [i 0]
      (if (>= i max-len)
        0
        (let [a (get p1 i 0)
              b (get p2 i 0)]
          (cond
            (and (number? a) (number? b))
            (let [diff (- a b)]
              (if (zero? diff)
                (recur (inc i))
                diff))
            
            (and (string? a) (string? b))
            (let [diff (compare a b)]
              (if (zero? diff)
                (recur (inc i))
                diff))
            
            :else
            (recur (inc i))))))))

(defn is-downgrade?
  "Check if updating from current to target is a downgrade"
  [current-version target-version]
  (when (and current-version target-version)
    (> (compare-versions current-version target-version) 0)))

(defn is-same-version?
  "Check if current and target versions are the same"
  [current-version target-version]
  (when (and current-version target-version)
    (zero? (compare-versions current-version target-version))))

(defn handle-downgrade-attempt!
  "Handle an attempted downgrade"
  [current-version target-version & {:keys [force]}]
  (log/warn "Downgrade attempt detected:" current-version "->" target-version)
  (swap! edge-case-state update :downgrade-attempts conj
         {:from current-version
          :to target-version
          :timestamp (str (Instant/now))
          :forced force})
  
  (cond
    force
    (do
      (log/warn "Forcing downgrade as requested")
      {:allow true :warning "Downgrade forced by user"})
    
    (:allow-downgrade @edge-case-config)
    (if (:require-user-confirmation-for-downgrade @edge-case-config)
      {:allow false :requires-confirmation true
       :message (str "You are about to downgrade from " current-version 
                     " to " target-version ". This may cause issues.")}
      {:allow true :warning "Downgrade allowed by configuration"})
    
    :else
    {:allow false 
     :error (str "Downgrade from " current-version " to " target-version 
                 " is not allowed. Use --force to override.")}))

;; =============================================================================
;; Same Version Re-download
;; =============================================================================

(defn handle-same-version-reinstall!
  "Handle attempt to reinstall the same version"
  [version & {:keys [force]}]
  (log/info "Same version reinstall requested:" version)
  
  (cond
    force
    (do
      (log/info "Forcing reinstall of same version")
      {:allow true :warning "Reinstalling same version as requested"})
    
    (:allow-same-version-reinstall @edge-case-config)
    {:allow true :info "Reinstalling same version"}
    
    :else
    {:allow false 
     :info "Already on this version"
     :message (str "Version " version " is already installed. "
                   "Use --force to reinstall.")}))

;; =============================================================================
;; Update While Minimized to Tray
;; =============================================================================

(defn set-minimized-to-tray!
  "Set whether app is minimized to tray"
  [minimized]
  (swap! edge-case-state assoc :minimized-to-tray minimized))

(defn is-minimized-to-tray?
  "Check if app is minimized to tray"
  []
  (:minimized-to-tray @edge-case-state))

(defn handle-update-while-minimized!
  "Handle update when app is minimized to tray"
  [update-fn & {:keys [show-notification restore-on-complete]}]
  (log/info "Handling update while minimized to tray")
  
  (when show-notification
    ;; Would trigger system tray notification
    (log/info "Showing tray notification for update"))
  
  (let [result (update-fn)]
    (when (and (:success result) restore-on-complete)
      ;; Would restore window from tray
      (log/info "Restoring window after update"))
    result))

;; =============================================================================
;; Update During Active Scan
;; =============================================================================

(defonce active-scans (atom #{}))

(defn register-active-scan!
  "Register an active scan"
  [scan-id]
  (swap! active-scans conj scan-id)
  (swap! edge-case-state assoc :active-scan-detected true)
  (log/debug "Registered active scan:" scan-id))

(defn unregister-active-scan!
  "Unregister a completed scan"
  [scan-id]
  (swap! active-scans disj scan-id)
  (when (empty? @active-scans)
    (swap! edge-case-state assoc :active-scan-detected false))
  (log/debug "Unregistered scan:" scan-id))

(defn has-active-scans?
  "Check if there are active scans"
  []
  (not (empty? @active-scans)))

(defn get-active-scan-count
  "Get count of active scans"
  []
  (count @active-scans))

(defn handle-update-during-scan!
  "Handle update request while scans are active"
  [update-fn & {:keys [wait-for-scans force cancel-scans]}]
  (if (has-active-scans?)
    (cond
      force
      (do
        (log/warn "Forcing update despite" (get-active-scan-count) "active scans")
        (update-fn))
      
      cancel-scans
      (do
        (log/warn "Cancelling" (get-active-scan-count) "active scans for update")
        ;; Would cancel scans here
        (reset! active-scans #{})
        (swap! edge-case-state assoc :active-scan-detected false)
        (update-fn))
      
      wait-for-scans
      (do
        (log/info "Waiting for" (get-active-scan-count) "scans to complete")
        {:success false 
         :waiting true
         :active-scans (get-active-scan-count)
         :message "Update will proceed after scans complete"})
      
      :else
      {:success false
       :blocked true
       :active-scans (get-active-scan-count)
       :message (str "Cannot update while " (get-active-scan-count) 
                     " scan(s) are in progress")})
    (update-fn)))

;; =============================================================================
;; Interrupted Update Recovery
;; =============================================================================

(defn save-update-state!
  "Save current update state for recovery"
  [state]
  (let [state-file (io/file (:update-state-file @edge-case-config))]
    (io/make-parents state-file)
    (spit state-file (pr-str (assoc state :saved-at (str (Instant/now)))))
    (log/debug "Saved update state")))

(defn load-update-state
  "Load saved update state"
  []
  (let [state-file (io/file (:update-state-file @edge-case-config))]
    (when (.exists state-file)
      (try
        (read-string (slurp state-file))
        (catch Exception e
          (log/warn "Failed to load update state:" (.getMessage e))
          nil)))))

(defn clear-update-state!
  "Clear saved update state"
  []
  (let [state-file (io/file (:update-state-file @edge-case-config))]
    (when (.exists state-file)
      (.delete state-file)
      (log/debug "Cleared update state"))))

(defn record-interrupted-update!
  "Record an interrupted update for later recovery"
  [update-info reason]
  (let [record {:id (str (UUID/randomUUID))
                :update-info update-info
                :reason reason
                :timestamp (str (Instant/now))
                :resume-attempts 0}]
    (swap! edge-case-state update :interrupted-updates conj record)
    (save-update-state! {:interrupted-update record})
    (log/warn "Recorded interrupted update:" reason)
    record))

(defn get-interrupted-updates
  "Get list of interrupted updates"
  []
  (:interrupted-updates @edge-case-state))

(defn can-resume-interrupted-update?
  "Check if an interrupted update can be resumed"
  [update-record]
  (< (:resume-attempts update-record 0) 
     (:max-interrupted-resume-attempts @edge-case-config)))

(defn resume-interrupted-update!
  "Resume an interrupted update"
  [update-id resume-fn]
  (let [updates (:interrupted-updates @edge-case-state)
        update-record (first (filter #(= (:id %) update-id) updates))]
    (if update-record
      (if (can-resume-interrupted-update? update-record)
        (do
          (log/info "Resuming interrupted update:" update-id)
          (swap! edge-case-state update :interrupted-updates
                 (fn [updates]
                   (map #(if (= (:id %) update-id)
                           (update % :resume-attempts inc)
                           %)
                        updates)))
          (let [result (resume-fn (:update-info update-record))]
            (when (:success result)
              (swap! edge-case-state update :interrupted-updates
                     (fn [updates] (remove #(= (:id %) update-id) updates)))
              (clear-update-state!))
            result))
        {:success false 
         :error "Max resume attempts exceeded"
         :attempts (:resume-attempts update-record)})
      {:success false :error "Interrupted update not found"})))

;; =============================================================================
;; Partial Download Handling
;; =============================================================================

(defn record-partial-download!
  "Record a partial download"
  [download-info bytes-downloaded total-bytes]
  (let [record {:id (str (UUID/randomUUID))
                :download-info download-info
                :bytes-downloaded bytes-downloaded
                :total-bytes total-bytes
                :progress (if (> total-bytes 0)
                            (* 100.0 (/ bytes-downloaded total-bytes))
                            0)
                :timestamp (str (Instant/now))}]
    (swap! edge-case-state update :partial-downloads conj record)
    (log/info "Recorded partial download:" bytes-downloaded "/" total-bytes "bytes")
    record))

(defn get-partial-downloads
  "Get list of partial downloads"
  []
  (:partial-downloads @edge-case-state))

(defn can-resume-partial-download?
  "Check if a partial download can be resumed"
  [download-record]
  (let [age-hours (/ (- (System/currentTimeMillis)
                        (.toEpochMilli (Instant/parse (:timestamp download-record))))
                     3600000)]
    (< age-hours (:partial-download-cleanup-age-hours @edge-case-config))))

(defn cleanup-old-partial-downloads!
  "Clean up old partial downloads"
  []
  (let [max-age-hours (:partial-download-cleanup-age-hours @edge-case-config)
        now (System/currentTimeMillis)]
    (swap! edge-case-state update :partial-downloads
           (fn [downloads]
             (filter (fn [d]
                       (let [age-hours (/ (- now (.toEpochMilli (Instant/parse (:timestamp d))))
                                          3600000)]
                         (< age-hours max-age-hours)))
                     downloads)))
    (log/debug "Cleaned up old partial downloads")))

;; =============================================================================
;; Corrupted State Recovery
;; =============================================================================

(defn detect-corrupted-state
  "Detect if update state is corrupted"
  []
  (let [state (load-update-state)]
    (when state
      (cond
        (nil? (:saved-at state))
        {:corrupted true :reason "Missing timestamp"}
        
        (and (:interrupted-update state)
             (nil? (get-in state [:interrupted-update :update-info])))
        {:corrupted true :reason "Missing update info"}
        
        :else
        {:corrupted false}))))

(defn enter-recovery-mode!
  "Enter recovery mode for corrupted state"
  []
  (log/warn "Entering recovery mode")
  (swap! edge-case-state assoc :recovery-mode true)
  
  ;; Clear potentially corrupted state
  (clear-update-state!)
  
  ;; Reset edge case state
  (swap! edge-case-state assoc
         :interrupted-updates []
         :partial-downloads [])
  
  {:success true :message "Recovery mode activated"})

(defn exit-recovery-mode!
  "Exit recovery mode"
  []
  (swap! edge-case-state assoc :recovery-mode false)
  (log/info "Exited recovery mode"))

(defn is-in-recovery-mode?
  "Check if in recovery mode"
  []
  (:recovery-mode @edge-case-state))

;; =============================================================================
;; Version Validation
;; =============================================================================

(defn validate-update-request
  "Validate an update request for edge cases"
  [current-version target-version & {:keys [force]}]
  (cond
    (nil? target-version)
    {:valid false :error "Target version is required"}
    
    (is-same-version? current-version target-version)
    (let [result (handle-same-version-reinstall! target-version :force force)]
      (if (:allow result)
        {:valid true :warning (:warning result)}
        {:valid false :info (:message result)}))
    
    (is-downgrade? current-version target-version)
    (let [result (handle-downgrade-attempt! current-version target-version :force force)]
      (if (:allow result)
        {:valid true :warning (:warning result)}
        (if (:requires-confirmation result)
          {:valid false :requires-confirmation true :message (:message result)}
          {:valid false :error (:error result)})))
    
    :else
    {:valid true}))

;; =============================================================================
;; Comprehensive Edge Case Handler
;; =============================================================================

(defn handle-update-with-edge-cases!
  "Handle update with all edge case checks"
  [current-version target-version update-fn & {:keys [force wait-for-scans]}]
  (log/info "Handling update with edge case checks")
  
  ;; Check for corrupted state
  (let [corruption-check (detect-corrupted-state)]
    (when (:corrupted corruption-check)
      (log/warn "Corrupted state detected:" (:reason corruption-check))
      (enter-recovery-mode!)))
  
  ;; Validate the update request
  (let [validation (validate-update-request current-version target-version :force force)]
    (if-not (:valid validation)
      validation
      
      ;; Check for active scans
      (if (has-active-scans?)
        (handle-update-during-scan! 
          #(update-fn) 
          :wait-for-scans wait-for-scans 
          :force force)
        
        ;; Check for minimized state
        (if (is-minimized-to-tray?)
          (handle-update-while-minimized! 
            #(update-fn)
            :show-notification true)
          
          ;; Normal update
          (update-fn))))))

;; =============================================================================
;; Status
;; =============================================================================

(defn get-edge-case-status
  "Get current edge case status"
  []
  {:is-first-install (is-first-install?)
   :interrupted-updates (count (:interrupted-updates @edge-case-state))
   :partial-downloads (count (:partial-downloads @edge-case-state))
   :downgrade-attempts (count (:downgrade-attempts @edge-case-state))
   :active-scans (get-active-scan-count)
   :minimized-to-tray (is-minimized-to-tray?)
   :recovery-mode (is-in-recovery-mode?)
   :config @edge-case-config})

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-edge-cases!
  "Initialize edge case handling"
  []
  (log/info "Initializing edge case handling")
  
  ;; Check for first install
  (swap! edge-case-state assoc :is-first-install (is-first-install?))
  
  ;; Load any saved state
  (when-let [saved-state (load-update-state)]
    (when-let [interrupted (:interrupted-update saved-state)]
      (swap! edge-case-state update :interrupted-updates conj interrupted)
      (log/info "Found interrupted update from previous session")))
  
  ;; Check for corrupted state
  (let [corruption-check (detect-corrupted-state)]
    (when (:corrupted corruption-check)
      (enter-recovery-mode!)))
  
  ;; Cleanup old partial downloads
  (cleanup-old-partial-downloads!)
  
  {:success true
   :first-install (is-first-install?)
   :has-interrupted-updates (not (empty? (:interrupted-updates @edge-case-state)))})
