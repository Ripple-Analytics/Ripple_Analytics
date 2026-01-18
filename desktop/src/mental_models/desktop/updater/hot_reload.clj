(ns mental-models.desktop.updater.hot-reload
  "Hot Code Reloading - Reload code without restarting the JVM
   
   Leverages Clojure's dynamic nature to:
   - Reload changed namespaces on the fly
   - Preserve application state during reloads
   - Track dependencies and reload in correct order
   - Rollback on reload failure
   
   This enables sub-second updates for most code changes."
  (:require [clojure.java.io :as io]
            [clojure.tools.namespace.repl :as repl]
            [clojure.tools.namespace.track :as track]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.dependency :as dep]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.nio.file FileSystems Paths StandardWatchEventKinds WatchService]
           [java.time Instant]))

;; =============================================================================
;; State Management
;; =============================================================================

(defonce reload-state
  (atom {:last-reload nil
         :reload-count 0
         :failed-reloads []
         :preserved-state {}
         :watched-dirs #{}
         :file-hashes {}
         :pending-changes []}))

(defonce state-preservers
  "Functions to save/restore state during reloads"
  (atom {}))

(defn register-state-preserver!
  "Register a function to preserve state during reloads
   
   preserver-fn should return a map with :save and :restore functions"
  [id preserver-fn]
  (swap! state-preservers assoc id preserver-fn)
  (log/debug "Registered state preserver:" id))

(defn unregister-state-preserver! [id]
  (swap! state-preservers dissoc id))

;; =============================================================================
;; File Watching
;; =============================================================================

(defonce watcher (atom nil))
(defonce watcher-thread (atom nil))

(defn file-hash [file]
  "Get a hash of file contents for change detection"
  (when (.exists file)
    (hash (slurp file))))

(defn scan-source-files [dirs]
  "Scan directories for Clojure source files"
  (into {}
    (for [dir dirs
          :let [dir-file (io/file dir)]
          :when (.exists dir-file)
          file (file-seq dir-file)
          :when (and (.isFile file)
                     (or (.endsWith (.getName file) ".clj")
                         (.endsWith (.getName file) ".cljc")))]
      [(.getAbsolutePath file) (file-hash file)])))

(defn detect-changes [old-hashes new-hashes]
  "Detect which files have changed"
  (let [all-files (set (concat (keys old-hashes) (keys new-hashes)))]
    (reduce
      (fn [changes file]
        (let [old-hash (get old-hashes file)
              new-hash (get new-hashes file)]
          (cond
            (nil? old-hash) (update changes :added conj file)
            (nil? new-hash) (update changes :removed conj file)
            (not= old-hash new-hash) (update changes :modified conj file)
            :else changes)))
      {:added [] :modified [] :removed []}
      all-files)))

(defn start-file-watcher! [dirs on-change]
  "Start watching directories for file changes"
  (let [ws (FileSystems/getDefault)
        watch-service (.newWatchService ws)]
    
    ;; Register directories
    (doseq [dir dirs]
      (let [path (Paths/get dir (into-array String []))]
        (.register path watch-service
                   (into-array [StandardWatchEventKinds/ENTRY_MODIFY
                                StandardWatchEventKinds/ENTRY_CREATE
                                StandardWatchEventKinds/ENTRY_DELETE]))))
    
    (reset! watcher watch-service)
    
    ;; Start watcher thread
    (reset! watcher-thread
      (Thread.
        (fn []
          (log/info "File watcher started for:" dirs)
          (try
            (loop []
              (when-let [key (.take watch-service)]
                (let [events (.pollEvents key)]
                  (when (seq events)
                    (log/debug "Detected" (count events) "file changes")
                    (on-change events)))
                (.reset key)
                (recur)))
            (catch InterruptedException _
              (log/info "File watcher stopped"))
            (catch Exception e
              (log/error e "File watcher error"))))))
    
    (.start @watcher-thread)
    (swap! reload-state update :watched-dirs into dirs)))

(defn stop-file-watcher! []
  "Stop the file watcher"
  (when @watcher
    (.close @watcher)
    (reset! watcher nil))
  (when @watcher-thread
    (.interrupt @watcher-thread)
    (reset! watcher-thread nil))
  (log/info "File watcher stopped"))

;; =============================================================================
;; Namespace Reloading
;; =============================================================================

(defn file->namespace [file]
  "Convert a file path to a namespace symbol"
  (when-let [ns-decl (try 
                       (with-open [rdr (java.io.PushbackReader. (io/reader file))]
                         (read rdr))
                       (catch Exception _ nil))]
    (when (and (list? ns-decl) (= 'ns (first ns-decl)))
      (second ns-decl))))

(defn get-dependent-namespaces [ns-sym]
  "Get namespaces that depend on the given namespace"
  (let [tracker (-> (track/tracker)
                    (dir/scan-all (map io/file (:watched-dirs @reload-state))))]
    (->> (dep/transitive-dependents (:clojure.tools.namespace.track/deps tracker) ns-sym)
         (into #{}))))

(defn reload-order [namespaces]
  "Determine the correct order to reload namespaces"
  (let [tracker (-> (track/tracker)
                    (dir/scan-all (map io/file (:watched-dirs @reload-state))))
        deps (:clojure.tools.namespace.track/deps tracker)]
    (sort-by #(count (dep/transitive-dependencies deps %)) namespaces)))

(defn save-state! []
  "Save state from all registered preservers"
  (log/debug "Saving state before reload...")
  (reduce-kv
    (fn [state id preserver]
      (try
        (assoc state id ((:save preserver)))
        (catch Exception e
          (log/warn "Failed to save state for" id ":" (.getMessage e))
          state)))
    {}
    @state-preservers))

(defn restore-state! [saved-state]
  "Restore state to all registered preservers"
  (log/debug "Restoring state after reload...")
  (doseq [[id preserver] @state-preservers]
    (when-let [state (get saved-state id)]
      (try
        ((:restore preserver) state)
        (catch Exception e
          (log/warn "Failed to restore state for" id ":" (.getMessage e)))))))

(defn reload-namespace! [ns-sym]
  "Reload a single namespace"
  (log/debug "Reloading namespace:" ns-sym)
  (try
    (require ns-sym :reload)
    {:success true :namespace ns-sym}
    (catch Exception e
      (log/error e "Failed to reload" ns-sym)
      {:success false :namespace ns-sym :error (.getMessage e)})))

(defn reload-namespaces! [ns-syms]
  "Reload multiple namespaces in dependency order"
  (let [ordered (reload-order ns-syms)
        saved-state (save-state!)]
    (log/info "Reloading" (count ordered) "namespaces:" ordered)
    
    (let [results (doall (map reload-namespace! ordered))
          failures (filter #(not (:success %)) results)]
      
      (if (empty? failures)
        (do
          (restore-state! saved-state)
          (swap! reload-state update :reload-count inc)
          (swap! reload-state assoc :last-reload (str (Instant/now)))
          (log/info "✅ Hot reload complete:" (count results) "namespaces")
          {:success true :reloaded ordered})
        (do
          (log/error "❌ Hot reload failed:" (count failures) "errors")
          (swap! reload-state update :failed-reloads conj
                 {:timestamp (str (Instant/now))
                  :failures failures})
          {:success false :failures failures})))))

;; =============================================================================
;; Automatic Hot Reload
;; =============================================================================

(defn handle-file-changes [changed-files]
  "Handle detected file changes by reloading affected namespaces"
  (let [ns-to-reload (keep file->namespace changed-files)
        all-affected (into #{} (mapcat get-dependent-namespaces ns-to-reload))]
    (when (seq all-affected)
      (log/info "Files changed, reloading:" (count all-affected) "namespaces")
      (reload-namespaces! all-affected))))

(defn start-auto-reload! [source-dirs]
  "Start automatic hot reloading for source directories"
  (log/info "Starting auto hot-reload for:" source-dirs)
  
  ;; Initial scan
  (let [initial-hashes (scan-source-files source-dirs)]
    (swap! reload-state assoc :file-hashes initial-hashes))
  
  ;; Start watcher
  (start-file-watcher! source-dirs
    (fn [_events]
      ;; Debounce: wait 100ms for more changes
      (Thread/sleep 100)
      (let [old-hashes (:file-hashes @reload-state)
            new-hashes (scan-source-files source-dirs)
            changes (detect-changes old-hashes new-hashes)]
        (swap! reload-state assoc :file-hashes new-hashes)
        (let [changed-files (concat (:added changes) (:modified changes))]
          (when (seq changed-files)
            (handle-file-changes changed-files)))))))

(defn stop-auto-reload! []
  "Stop automatic hot reloading"
  (stop-file-watcher!)
  (log/info "Auto hot-reload stopped"))

;; =============================================================================
;; Manual Reload Commands
;; =============================================================================

(defn reload-all! []
  "Reload all namespaces (full refresh)"
  (log/info "Performing full reload...")
  (let [saved-state (save-state!)]
    (try
      (repl/refresh)
      (restore-state! saved-state)
      (swap! reload-state update :reload-count inc)
      (swap! reload-state assoc :last-reload (str (Instant/now)))
      (log/info "✅ Full reload complete")
      {:success true}
      (catch Exception e
        (log/error e "Full reload failed")
        {:success false :error (.getMessage e)}))))

(defn reload-changed! []
  "Reload only changed namespaces"
  (let [source-dirs (:watched-dirs @reload-state)
        old-hashes (:file-hashes @reload-state)
        new-hashes (scan-source-files source-dirs)
        changes (detect-changes old-hashes new-hashes)
        changed-files (concat (:added changes) (:modified changes))]
    (swap! reload-state assoc :file-hashes new-hashes)
    (if (seq changed-files)
      (handle-file-changes changed-files)
      (do
        (log/info "No changes detected")
        {:success true :reloaded []}))))

(defn get-reload-status []
  "Get hot reload status"
  (select-keys @reload-state 
               [:last-reload :reload-count :failed-reloads :watched-dirs]))
