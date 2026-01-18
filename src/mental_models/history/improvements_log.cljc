(ns mental-models.history.improvements-log
  "Git-style Improvements Log for Mental Models
   
   Track changes to mental models over time with:
   - Commit history with hash, message, author, timestamp
   - Branch tags for different versions
   - Diff view for changes
   - Track improvements at a glance
   
   Similar to Git commit history UI"
  (:require
   [clojure.string :as str]
   #?(:clj [clojure.java.io :as io])
   #?(:clj [clojure.edn :as edn])
   #?(:clj [hyperfiddle.electric :as e])))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(def ^:dynamic *log-file* ".mental-models-history.edn")

;; =============================================================================
;; STATE
;; =============================================================================

(defonce improvements-log
  (atom {:commits []
         :branches {"main" {:head nil :commits []}}
         :current-branch "main"
         :tags {}}))

;; =============================================================================
;; COMMIT HASH GENERATION
;; =============================================================================

#?(:clj
   (defn generate-hash
     "Generate a short hash for a commit (like Git's short SHA)"
     [content]
     (let [md (java.security.MessageDigest/getInstance "SHA-1")
           bytes (.digest md (.getBytes (str content (System/currentTimeMillis))))]
       (subs (apply str (map #(format "%02x" %) bytes)) 0 7))))

;; =============================================================================
;; DIFF GENERATION
;; =============================================================================

(defn diff-maps
  "Generate diff between two maps (old and new)"
  [old-map new-map]
  (let [all-keys (set (concat (keys old-map) (keys new-map)))
        changes (for [k all-keys
                      :let [old-val (get old-map k)
                            new-val (get new-map k)]
                      :when (not= old-val new-val)]
                  {:key k
                   :old old-val
                   :new new-val
                   :type (cond
                           (nil? old-val) :added
                           (nil? new-val) :removed
                           :else :modified)})]
    (vec changes)))

(defn format-diff-line
  "Format a single diff line"
  [{:keys [key old new type]}]
  (case type
    :added (str "+ " key ": " new)
    :removed (str "- " key ": " old)
    :modified (str "~ " key ": " old " -> " new)))

(defn format-diff
  "Format diff for display"
  [changes]
  (str/join "\n" (map format-diff-line changes)))

;; =============================================================================
;; COMMIT OPERATIONS
;; =============================================================================

#?(:clj
   (defn create-commit
     "Create a new commit for a mental model change"
     [model-id message changes & {:keys [author branch]
                                   :or {author "system" branch "main"}}]
     (let [timestamp (System/currentTimeMillis)
           parent (get-in @improvements-log [:branches branch :head])
           commit {:hash (generate-hash (str model-id message changes timestamp))
                   :model-id model-id
                   :message message
                   :author author
                   :timestamp timestamp
                   :parent parent
                   :branch branch
                   :changes changes}]
       (swap! improvements-log
              (fn [log]
                (-> log
                    (update :commits conj commit)
                    (update-in [:branches branch :commits] conj (:hash commit))
                    (assoc-in [:branches branch :head] (:hash commit)))))
       commit)))

#?(:clj
   (defn record-improvement
     "Record an improvement to a mental model"
     [model-id old-state new-state message & {:keys [author] :or {author "system"}}]
     (let [changes (diff-maps old-state new-state)]
       (when (seq changes)
         (create-commit model-id message changes :author author)))))

;; =============================================================================
;; BRANCH OPERATIONS
;; =============================================================================

#?(:clj
   (defn create-branch
     "Create a new branch from current HEAD"
     [branch-name]
     (let [current-branch (:current-branch @improvements-log)
           head (get-in @improvements-log [:branches current-branch :head])]
       (swap! improvements-log assoc-in [:branches branch-name]
              {:head head :commits [head]})
       branch-name)))

#?(:clj
   (defn switch-branch
     "Switch to a different branch"
     [branch-name]
     (when (get-in @improvements-log [:branches branch-name])
       (swap! improvements-log assoc :current-branch branch-name)
       branch-name)))

;; =============================================================================
;; TAG OPERATIONS
;; =============================================================================

#?(:clj
   (defn create-tag
     "Create a tag at current HEAD"
     [tag-name & {:keys [message]}]
     (let [current-branch (:current-branch @improvements-log)
           head (get-in @improvements-log [:branches current-branch :head])]
       (swap! improvements-log assoc-in [:tags tag-name]
              {:hash head :message message :timestamp (System/currentTimeMillis)})
       tag-name)))

;; =============================================================================
;; QUERY OPERATIONS
;; =============================================================================

(defn get-commit
  "Get a commit by hash"
  [hash]
  (first (filter #(= hash (:hash %)) (:commits @improvements-log))))

(defn get-model-history
  "Get commit history for a specific model"
  [model-id & {:keys [limit] :or {limit 50}}]
  (->> (:commits @improvements-log)
       (filter #(= model-id (:model-id %)))
       (sort-by :timestamp >)
       (take limit)))

(defn get-branch-history
  "Get commit history for a branch"
  [branch-name & {:keys [limit] :or {limit 50}}]
  (let [commit-hashes (get-in @improvements-log [:branches branch-name :commits])]
    (->> (:commits @improvements-log)
         (filter #(contains? (set commit-hashes) (:hash %)))
         (sort-by :timestamp >)
         (take limit))))

(defn get-all-history
  "Get all commit history"
  [& {:keys [limit] :or {limit 100}}]
  (->> (:commits @improvements-log)
       (sort-by :timestamp >)
       (take limit)))

(defn get-tags-for-commit
  "Get all tags pointing to a commit"
  [hash]
  (->> (:tags @improvements-log)
       (filter (fn [[_ v]] (= hash (:hash v))))
       (map first)))

;; =============================================================================
;; PERSISTENCE
;; =============================================================================

#?(:clj
   (defn save-log!
     "Save improvements log to file"
     []
     (spit *log-file* (pr-str @improvements-log))
     (println "[HISTORY] Saved" (count (:commits @improvements-log)) "commits")))

#?(:clj
   (defn load-log!
     "Load improvements log from file"
     []
     (try
       (when (.exists (io/file *log-file*))
         (reset! improvements-log (edn/read-string (slurp *log-file*)))
         (println "[HISTORY] Loaded" (count (:commits @improvements-log)) "commits"))
       (catch Exception e
         (println "[HISTORY] Load failed:" (.getMessage e))))))

;; =============================================================================
;; FORMATTING FOR DISPLAY
;; =============================================================================

(defn format-timestamp
  "Format timestamp for display"
  [ts]
  #?(:clj (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
            (.format fmt (java.util.Date. ts)))
     :cljs (let [d (js/Date. ts)]
             (.toISOString d))))

(defn format-commit-oneline
  "Format commit as single line (like git log --oneline)"
  [commit]
  (let [tags (get-tags-for-commit (:hash commit))
        tag-str (when (seq tags) (str " (" (str/join ", " tags) ")"))]
    (str (:hash commit) " " (:message commit) tag-str)))

(defn format-commit-full
  "Format commit with full details"
  [commit]
  (let [tags (get-tags-for-commit (:hash commit))]
    (str "commit " (:hash commit)
         (when (seq tags) (str " (" (str/join ", " tags) ")"))
         "\nAuthor: " (:author commit)
         "\nDate:   " (format-timestamp (:timestamp commit))
         "\nModel:  " (:model-id commit)
         "\n\n    " (:message commit)
         "\n\n" (format-diff (:changes commit)))))

;; =============================================================================
;; ELECTRIC REACTIVE COMPONENTS
;; =============================================================================

#?(:clj
   (e/defn ImprovementsLog []
     (e/server
      (let [log (e/watch improvements-log)
            commits (take 20 (sort-by :timestamp > (:commits log)))]
        (e/client
         [:div.improvements-log
          [:h2 "Improvements Log"]
          [:div.branch-info
           [:span.current-branch "Branch: " (:current-branch log)]
           [:span.commit-count (str (count (:commits log)) " commits")]]
          [:div.commit-list
           (e/for [commit commits]
             [:div.commit-item
              [:div.commit-header
               [:span.hash (:hash commit)]
               [:span.message (:message commit)]
               (e/for [tag (get-tags-for-commit (:hash commit))]
                 [:span.tag tag])]
              [:div.commit-meta
               [:span.author (:author commit)]
               [:span.date (format-timestamp (:timestamp commit))]
               [:span.model (:model-id commit)]]
              [:div.commit-diff
               (e/for [change (:changes commit)]
                 [:div.diff-line {:class (name (:type change))}
                  (format-diff-line change)])]])]])))))

#?(:clj
   (e/defn ModelHistory [model-id]
     (e/server
      (let [history (get-model-history model-id :limit 10)]
        (e/client
         [:div.model-history
          [:h3 (str "History: " model-id)]
          (if (empty? history)
            [:p.no-history "No changes recorded"]
            [:ul.history-list
             (e/for [commit history]
               [:li.history-item
                [:span.hash (:hash commit)]
                [:span.message (:message commit)]
                [:span.date (format-timestamp (:timestamp commit))]])])])))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

#?(:clj
   (defn init!
     "Initialize improvements log"
     []
     (load-log!)
     (println "[HISTORY] Improvements log initialized")))
