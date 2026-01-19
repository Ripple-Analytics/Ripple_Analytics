(ns mental-models.desktop.updater.version
  "Semantic Version Management - Bulletproof version handling
   
   Features:
   - Semantic version parsing (v2.3.0, 2.3.1, v2.10.0)
   - Version comparison with proper numeric ordering
   - Malformed version handling with graceful fallbacks
   - Version display formatting
   - Build metadata and pre-release support
   - Version history tracking
   - UI integration helpers"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.time Instant]
           [java.util.regex Pattern]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:version-file (str (System/getProperty "user.home") "/.mental-models/version.edn")
         :config-file (str (System/getProperty "user.home") "/.mental-models/config.edn")
         :fallback-version "0.0.0"
         :version-pattern #"^v?(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.-]+))?(?:\+([a-zA-Z0-9.-]+))?$"}))

(defonce version-state
  (atom {:current-version nil
         :latest-version nil
         :version-history []
         :last-check nil
         :update-available false
         :up-to-date false}))

;; =============================================================================
;; Version Parsing
;; =============================================================================

(defrecord SemanticVersion [major minor patch pre-release build-metadata raw])

(defn parse-version
  "Parse a version string into a SemanticVersion record.
   Handles formats: v2.3.0, 2.3.1, v2.10.0-beta.1, 2.0.0+build.123
   Returns nil for malformed versions."
  [version-str]
  (when (and version-str (string? version-str))
    (let [trimmed (str/trim version-str)
          pattern (:version-pattern @config)
          matcher (re-matcher pattern trimmed)]
      (if (re-find matcher)
        (let [[_ major minor patch pre-release build-meta] (re-matches pattern trimmed)]
          (try
            (->SemanticVersion
              (Integer/parseInt major)
              (Integer/parseInt minor)
              (Integer/parseInt patch)
              pre-release
              build-meta
              trimmed)
            (catch NumberFormatException e
              (log/warn "Failed to parse version numbers:" trimmed (.getMessage e))
              nil)))
        (do
          (log/debug "Version string doesn't match pattern:" trimmed)
          nil)))))

(defn valid-version?
  "Check if a version string is valid."
  [version-str]
  (some? (parse-version version-str)))

(defn normalize-version
  "Normalize a version string (remove 'v' prefix, trim whitespace)."
  [version-str]
  (when version-str
    (-> version-str
        str/trim
        (str/replace #"^v" ""))))

(defn version->string
  "Convert a SemanticVersion to a display string."
  [version & {:keys [include-v] :or {include-v true}}]
  (when version
    (let [base (str (when include-v "v")
                    (:major version) "."
                    (:minor version) "."
                    (:patch version))]
      (cond-> base
        (:pre-release version) (str "-" (:pre-release version))
        (:build-metadata version) (str "+" (:build-metadata version))))))

;; =============================================================================
;; Version Comparison
;; =============================================================================

(defn compare-pre-release
  "Compare pre-release identifiers.
   Rules:
   - No pre-release > any pre-release (1.0.0 > 1.0.0-alpha)
   - Numeric identifiers compared as integers
   - Alphanumeric identifiers compared lexically
   - Numeric < alphanumeric"
  [pr1 pr2]
  (cond
    (and (nil? pr1) (nil? pr2)) 0
    (nil? pr1) 1   ;; No pre-release is greater
    (nil? pr2) -1
    :else
    (let [parts1 (str/split pr1 #"\.")
          parts2 (str/split pr2 #"\.")
          max-len (max (count parts1) (count parts2))]
      (loop [i 0]
        (if (>= i max-len)
          0
          (let [p1 (get parts1 i)
                p2 (get parts2 i)]
            (cond
              (and (nil? p1) (nil? p2)) 0
              (nil? p1) -1  ;; Shorter is less
              (nil? p2) 1
              :else
              (let [n1 (try (Integer/parseInt p1) (catch Exception _ nil))
                    n2 (try (Integer/parseInt p2) (catch Exception _ nil))
                    cmp (cond
                          (and n1 n2) (compare n1 n2)
                          (and n1 (not n2)) -1  ;; Numeric < alphanumeric
                          (and (not n1) n2) 1
                          :else (compare p1 p2))]
                (if (zero? cmp)
                  (recur (inc i))
                  cmp)))))))))

(defn compare-versions
  "Compare two versions. Returns:
   - negative if v1 < v2
   - zero if v1 = v2
   - positive if v1 > v2
   
   Handles malformed versions gracefully by treating them as 0.0.0"
  [v1 v2]
  (let [ver1 (if (instance? SemanticVersion v1) v1 (or (parse-version v1) (parse-version "0.0.0")))
        ver2 (if (instance? SemanticVersion v2) v2 (or (parse-version v2) (parse-version "0.0.0")))]
    (let [major-cmp (compare (:major ver1) (:major ver2))]
      (if (not (zero? major-cmp))
        major-cmp
        (let [minor-cmp (compare (:minor ver1) (:minor ver2))]
          (if (not (zero? minor-cmp))
            minor-cmp
            (let [patch-cmp (compare (:patch ver1) (:patch ver2))]
              (if (not (zero? patch-cmp))
                patch-cmp
                (compare-pre-release (:pre-release ver1) (:pre-release ver2))))))))))

(defn version>
  "Check if v1 is greater than v2."
  [v1 v2]
  (pos? (compare-versions v1 v2)))

(defn version<
  "Check if v1 is less than v2."
  [v1 v2]
  (neg? (compare-versions v1 v2)))

(defn version=
  "Check if v1 equals v2."
  [v1 v2]
  (zero? (compare-versions v1 v2)))

(defn version>=
  "Check if v1 is greater than or equal to v2."
  [v1 v2]
  (>= (compare-versions v1 v2) 0))

(defn version<=
  "Check if v1 is less than or equal to v2."
  [v1 v2]
  (<= (compare-versions v1 v2) 0))

;; =============================================================================
;; Version File Management
;; =============================================================================

(defn read-version-file
  "Read version information from the version file.
   Returns nil if file doesn't exist or is corrupted."
  []
  (let [version-file (io/file (:version-file @config))]
    (when (.exists version-file)
      (try
        (let [content (slurp version-file)]
          (if (str/blank? content)
            nil
            (read-string content)))
        (catch Exception e
          (log/warn "Failed to read version file:" (.getMessage e))
          nil)))))

(defn write-version-file!
  "Write version information to the version file."
  [version-info]
  (let [version-file (io/file (:version-file @config))]
    (try
      (io/make-parents version-file)
      (spit version-file (pr-str version-info))
      (log/debug "Wrote version file:" version-info)
      true
      (catch Exception e
        (log/error "Failed to write version file:" (.getMessage e))
        false))))

(defn get-current-version
  "Get the current installed version.
   Falls back to config file, then to fallback version."
  []
  (or (:current-version @version-state)
      (let [file-data (read-version-file)]
        (:version file-data))
      (let [config-file (io/file (:config-file @config))]
        (when (.exists config-file)
          (try
            (:version (read-string (slurp config-file)))
            (catch Exception _ nil))))
      (:fallback-version @config)))

(defn set-current-version!
  "Set the current version and persist to file."
  [version]
  (let [parsed (parse-version version)
        version-str (if parsed (version->string parsed :include-v false) version)
        version-info {:version version-str
                      :updated-at (str (Instant/now))
                      :previous-version (get-current-version)}]
    (swap! version-state assoc :current-version version-str)
    (swap! version-state update :version-history conj
           {:version version-str
            :timestamp (str (Instant/now))})
    (write-version-file! version-info)
    (log/info "Version updated to:" version-str)))

;; =============================================================================
;; Update Detection
;; =============================================================================

(defn check-update-available
  "Check if an update is available.
   Returns a map with :available, :current, :latest, :is-downgrade"
  [latest-version]
  (let [current (get-current-version)
        current-parsed (parse-version current)
        latest-parsed (parse-version latest-version)]
    (cond
      (nil? latest-parsed)
      {:available false
       :current current
       :latest latest-version
       :error "Invalid latest version format"}
      
      (nil? current-parsed)
      {:available true
       :current current
       :latest latest-version
       :is-downgrade false
       :note "Current version unparseable, treating as update"}
      
      :else
      (let [cmp (compare-versions latest-parsed current-parsed)]
        {:available (pos? cmp)
         :current current
         :latest latest-version
         :is-downgrade (neg? cmp)
         :is-same (zero? cmp)}))))

(defn update-available?
  "Simple check if update is available."
  [latest-version]
  (:available (check-update-available latest-version)))

(defn is-up-to-date?
  "Check if current version is up to date."
  [latest-version]
  (let [result (check-update-available latest-version)]
    (or (:is-same result) (not (:available result)))))

;; =============================================================================
;; UI Integration
;; =============================================================================

(defn get-version-display
  "Get version string for UI display (title bar, etc.)."
  []
  (let [current (get-current-version)
        parsed (parse-version current)]
    (if parsed
      (version->string parsed :include-v true)
      (str "v" current))))

(defn get-update-status-display
  "Get update status for UI display."
  [latest-version]
  (let [result (check-update-available latest-version)]
    (cond
      (:error result) {:status :error :message (:error result)}
      (:is-same result) {:status :up-to-date :message "Up to date"}
      (:is-downgrade result) {:status :downgrade :message (str "Downgrade available: " (:latest result))}
      (:available result) {:status :update-available :message (str "Update available: " (:latest result))}
      :else {:status :up-to-date :message "Up to date"})))

(defn format-version-info
  "Format version info for display."
  []
  (let [current (get-current-version)
        parsed (parse-version current)
        file-data (read-version-file)]
    {:display (get-version-display)
     :version current
     :major (when parsed (:major parsed))
     :minor (when parsed (:minor parsed))
     :patch (when parsed (:patch parsed))
     :pre-release (when parsed (:pre-release parsed))
     :build-metadata (when parsed (:build-metadata parsed))
     :updated-at (:updated-at file-data)
     :previous-version (:previous-version file-data)}))

;; =============================================================================
;; Version History
;; =============================================================================

(defn get-version-history
  "Get version history."
  []
  (:version-history @version-state))

(defn clear-version-history!
  "Clear version history."
  []
  (swap! version-state assoc :version-history []))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-version-manager!
  "Initialize the version manager."
  []
  (log/info "Initializing version manager...")
  (let [current (get-current-version)]
    (swap! version-state assoc :current-version current)
    (log/info "Current version:" current)
    {:success true :version current}))

;; =============================================================================
;; Testing Helpers
;; =============================================================================

(defn test-version-comparison
  "Test version comparison logic."
  []
  (let [tests [["1.0.0" "1.0.0" 0]
               ["1.0.0" "1.0.1" -1]
               ["1.0.1" "1.0.0" 1]
               ["1.0.0" "1.1.0" -1]
               ["1.1.0" "1.0.0" 1]
               ["1.0.0" "2.0.0" -1]
               ["2.0.0" "1.0.0" 1]
               ["v1.0.0" "1.0.0" 0]
               ["2.3.0" "2.3.1" -1]
               ["2.3.1" "2.3.0" 1]
               ["2.3.0" "2.10.0" -1]  ;; Important: 10 > 3
               ["2.10.0" "2.3.0" 1]
               ["1.0.0-alpha" "1.0.0" -1]
               ["1.0.0" "1.0.0-alpha" 1]
               ["1.0.0-alpha" "1.0.0-beta" -1]
               ["1.0.0-alpha.1" "1.0.0-alpha.2" -1]]]
    (doseq [[v1 v2 expected] tests]
      (let [result (compare-versions v1 v2)
            sign (cond (pos? result) 1 (neg? result) -1 :else 0)]
        (if (= sign expected)
          (log/debug "PASS:" v1 "vs" v2 "=" sign)
          (log/error "FAIL:" v1 "vs" v2 "expected" expected "got" sign))))))
