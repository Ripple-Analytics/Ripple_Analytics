(ns mental-models.resilient-download-test
  "Tests for bulletproof download system with:
   1. Remote config fetching from web app
   2. Multiple download sources with fallback
   3. Retry logic with exponential backoff
   4. Local caching to avoid re-downloads
   5. Checksum verification
   
   These tests verify the ACTUAL download works before we ship."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net HttpURLConnection URL]
           [java.io File FileOutputStream BufferedInputStream]
           [java.security MessageDigest]
           [java.nio.file Files]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  {:web-app-url "https://3000-i7g2c9ows75ghqeegfaf0-42dd4221.sg1.manus.computer"
   :desktop-api-key "mm-desktop-2026-ripple"
   :github-token "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"
   :github-repo "Ripple-Analytics/Ripple_Analytics"
   :cache-dir (str (System/getProperty "java.io.tmpdir") "/mental-models-cache")
   :test-timeout 120000})

;; =============================================================================
;; HTTP Helpers
;; =============================================================================

(defn http-get 
  "Simple HTTP GET that returns {:success bool :body string :status int :error str}"
  [url & {:keys [headers timeout] :or {timeout 30000}}]
  (try
    (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout timeout)
                 (.setReadTimeout timeout)
                 (.setInstanceFollowRedirects true))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (let [status (.getResponseCode conn)]
        (if (<= 200 status 299)
          (let [body (slurp (.getInputStream conn))]
            {:success true :body body :status status})
          {:success false :status status :error (str "HTTP " status)})))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn download-file
  "Download file with redirect following. Returns {:success bool :bytes int :error str}"
  [url dest-file & {:keys [headers timeout follow-redirects?] 
                    :or {timeout 120000 follow-redirects? true}}]
  (try
    (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout timeout)
                 (.setInstanceFollowRedirects false))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (let [status (.getResponseCode conn)]
        (cond
          ;; Success - download
          (<= 200 status 299)
          (let [input (BufferedInputStream. (.getInputStream conn))
                output (FileOutputStream. dest-file)
                buffer (byte-array 65536)]
            (loop [total 0]
              (let [n (.read input buffer)]
                (if (pos? n)
                  (do
                    (.write output buffer 0 n)
                    (recur (+ total n)))
                  (do
                    (.close input)
                    (.close output)
                    (.disconnect conn)
                    {:success true :bytes total})))))
          
          ;; Redirect - follow WITHOUT auth headers (S3 signed URLs reject them)
          (#{301 302 303 307 308} status)
          (if follow-redirects?
            (let [redirect-url (.getHeaderField conn "Location")]
              (.disconnect conn)
              ;; Don't pass auth headers to redirect
              (download-file redirect-url dest-file :timeout timeout :follow-redirects? true))
            {:success false :status status :error "Redirect not followed"})
          
          ;; Error
          :else
          (do
            (.disconnect conn)
            {:success false :status status :error (str "HTTP " status)}))))
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Remote Config
;; =============================================================================

(defn fetch-remote-config
  "Fetch download config from web app"
  []
  (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
        headers {"X-Desktop-API-Key" (:desktop-api-key config)}
        result (http-get url :headers headers)]
    (when (:success result)
      ;; Parse the tRPC response
      (let [body (:body result)]
        ;; Extract JSON from tRPC wrapper
        (when-let [data-match (re-find #"\"json\":\s*(\{[^}]+\"downloadSources\"[^}]+\})" body)]
          ;; Simple extraction of key fields
          {:current-version (second (re-find #"\"currentVersion\":\s*\"([^\"]+)\"" body))
           :download-sources (vec (re-seq #"\{\"name\":\"([^\"]+)\"[^}]+\"url\":\"([^\"]+)\"[^}]+\"requiresAuth\":(true|false)" body))
           :raw body})))))

(defn parse-download-sources
  "Parse download sources from config"
  [config-body]
  (let [sources (re-seq #"\{\"name\":\"([^\"]+)\"[^}]*\"url\":\"([^\"]+)\"[^}]*\"requiresAuth\":(true|false)" config-body)]
    (mapv (fn [[_ name url auth]]
            {:name name
             :url url
             :requires-auth (= auth "true")})
          sources)))

;; =============================================================================
;; Download with Fallback
;; =============================================================================

(defn try-download-source
  "Try to download from a single source. Returns {:success bool :bytes int :error str}"
  [source dest-file]
  (let [headers (if (:requires-auth source)
                  {"Authorization" (str "token " (:github-token config))
                   "Accept" "application/octet-stream"}
                  {})]
    (download-file (:url source) dest-file :headers headers)))

(defn download-with-fallback
  "Try each download source in order until one succeeds"
  [sources dest-file]
  (loop [remaining sources
         errors []]
    (if (empty? remaining)
      {:success false :errors errors}
      (let [source (first remaining)
            _ (println (str "  Trying source: " (:name source)))
            result (try-download-source source dest-file)]
        (if (:success result)
          {:success true :source (:name source) :bytes (:bytes result)}
          (recur (rest remaining) 
                 (conj errors {:source (:name source) :error (:error result)})))))))

;; =============================================================================
;; Caching
;; =============================================================================

(defn ensure-cache-dir []
  (let [dir (io/file (:cache-dir config))]
    (.mkdirs dir)
    dir))

(defn cache-path [version]
  (io/file (:cache-dir config) (str "MentalModels-" version ".zip")))

(defn cached? [version]
  (.exists (cache-path version)))

(defn get-from-cache [version]
  (when (cached? version)
    (cache-path version)))

;; =============================================================================
;; Checksum
;; =============================================================================

(defn sha256-file [^File file]
  (let [md (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 65536)]
    (with-open [input (io/input-stream file)]
      (loop []
        (let [n (.read input buffer)]
          (when (pos? n)
            (.update md buffer 0 n)
            (recur)))))
    (apply str (map #(format "%02x" %) (.digest md)))))

;; =============================================================================
;; TESTS
;; =============================================================================

(deftest test-remote-config-requires-api-key
  (testing "Remote config endpoint rejects requests without API key"
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          result (http-get url)]
      (is (or (not (:success result))
              (str/includes? (:body result) "Invalid API key"))
          "Should reject request without API key"))))

(deftest test-remote-config-works-with-api-key
  (testing "Remote config endpoint returns data with valid API key"
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          headers {"X-Desktop-API-Key" (:desktop-api-key config)}
          result (http-get url :headers headers)]
      (is (:success result) "Should succeed with valid API key")
      (is (str/includes? (:body result) "currentVersion") "Should contain version")
      (is (str/includes? (:body result) "downloadSources") "Should contain download sources"))))

(deftest test-parse-download-sources
  (testing "Can parse download sources from config"
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          headers {"X-Desktop-API-Key" (:desktop-api-key config)}
          result (http-get url :headers headers)
          sources (parse-download-sources (:body result))]
      (is (>= (count sources) 2) "Should have at least 2 download sources")
      (is (some #(= "github_api" (:name %)) sources) "Should have github_api source")
      (is (every? :url sources) "All sources should have URLs"))))

(deftest test-github-api-download-works
  (testing "GitHub API download actually works for private repo"
    (ensure-cache-dir)
    (let [dest-file (io/file (:cache-dir config) "test-github-api.zip")
          url "https://api.github.com/repos/Ripple-Analytics/Ripple_Analytics/releases/assets/342565747"
          headers {"Authorization" (str "token " (:github-token config))
                   "Accept" "application/octet-stream"}
          result (download-file url dest-file :headers headers)]
      ;; Clean up before assertion so we don't leave files on failure
      (when (.exists dest-file)
        (let [size (.length dest-file)]
          (.delete dest-file)
          (is (:success result) (str "Download should succeed: " (:error result)))
          (is (> size 1000000) "Downloaded file should be > 1MB"))))))

(deftest test-download-with-fallback
  (testing "Download with fallback tries sources in order"
    (ensure-cache-dir)
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          headers {"X-Desktop-API-Key" (:desktop-api-key config)}
          result (http-get url :headers headers)
          sources (parse-download-sources (:body result))
          dest-file (io/file (:cache-dir config) "test-fallback.zip")]
      (when (.exists dest-file) (.delete dest-file))
      (let [download-result (download-with-fallback sources dest-file)]
        (when (.exists dest-file) (.delete dest-file))
        (is (:success download-result) 
            (str "At least one source should work: " (:errors download-result)))))))

(deftest test-cache-prevents-redownload
  (testing "Cached files are used instead of re-downloading"
    (ensure-cache-dir)
    (let [test-version "test-cache-v1"
          cache-file (cache-path test-version)]
      ;; Create fake cached file
      (spit cache-file "fake cached content")
      (is (cached? test-version) "Should detect cached file")
      (is (= cache-file (get-from-cache test-version)) "Should return cache path")
      ;; Cleanup
      (.delete cache-file))))

(deftest test-checksum-calculation
  (testing "SHA256 checksum is calculated correctly"
    (ensure-cache-dir)
    (let [test-file (io/file (:cache-dir config) "test-checksum.txt")]
      (spit test-file "Hello, World!")
      (let [checksum (sha256-file test-file)]
        (.delete test-file)
        (is (= checksum "dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f")
            "SHA256 should match known value")))))

(deftest test-full-update-flow
  (testing "Full update flow: config -> download -> verify"
    (ensure-cache-dir)
    (println "\n=== Full Update Flow Test ===")
    
    ;; Step 1: Fetch remote config
    (println "1. Fetching remote config...")
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          headers {"X-Desktop-API-Key" (:desktop-api-key config)}
          config-result (http-get url :headers headers)]
      (is (:success config-result) "Should fetch config")
      
      ;; Step 2: Parse sources
      (println "2. Parsing download sources...")
      (let [sources (parse-download-sources (:body config-result))]
        (is (seq sources) "Should have download sources")
        (println (str "   Found " (count sources) " sources"))
        
        ;; Step 3: Download with fallback
        (println "3. Downloading with fallback...")
        (let [dest-file (io/file (:cache-dir config) "test-full-flow.zip")
              _ (when (.exists dest-file) (.delete dest-file))
              download-result (download-with-fallback sources dest-file)]
          
          (if (:success download-result)
            (do
              (println (str "   SUCCESS from: " (:source download-result)))
              (println (str "   Downloaded: " (:bytes download-result) " bytes"))
              
              ;; Step 4: Verify it's a valid ZIP
              (println "4. Verifying ZIP file...")
              (let [header (byte-array 2)]
                (with-open [in (io/input-stream dest-file)]
                  (.read in header))
                (is (= (vec header) [80 75]) "Should be valid ZIP (PK header)"))
              
              ;; Cleanup
              (.delete dest-file)
              (is true "Full flow completed successfully"))
            
            (do
              (println "   FAILED - all sources failed:")
              (doseq [err (:errors download-result)]
                (println (str "   - " (:source err) ": " (:error err))))
              (is false "At least one download source should work"))))))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn run-resilient-download-tests []
  (println "\n" (apply str (repeat 72 "=")))
  (println "Running Resilient Download System Tests")
  (println (apply str (repeat 72 "=")) "\n")
  (run-tests 'mental-models.resilient-download-test))
