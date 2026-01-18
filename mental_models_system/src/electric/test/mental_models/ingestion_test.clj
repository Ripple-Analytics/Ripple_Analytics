(ns mental-models.ingestion-test
  "Tests for the ingestion module.
   
   Tests text chunking, file processing, pipeline,
   and feedback collection."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.ingestion :as ingestion]
            [clojure.java.io :as io]))

;; ============================================
;; Configuration Tests
;; ============================================

(deftest test-default-config
  (testing "Default configuration is defined"
    (is (map? ingestion/default-config))
    (is (contains? ingestion/default-config :chunk-size))
    (is (contains? ingestion/default-config :chunk-overlap))
    (is (contains? ingestion/default-config :supported-extensions))
    (is (contains? ingestion/default-config :batch-size))
    (is (number? (:chunk-size ingestion/default-config)))
    (is (number? (:chunk-overlap ingestion/default-config)))))

;; ============================================
;; Text Chunking Tests
;; ============================================

(deftest test-chunk-text-short
  (testing "Short text returns single chunk"
    (let [text "Short text"
          chunks (ingestion/chunk-text text)]
      (is (= 1 (count chunks)))
      (is (= text (:text (first chunks))))
      (is (= 0 (:start (first chunks))))
      (is (= (count text) (:end (first chunks)))))))

(deftest test-chunk-text-long
  (testing "Long text is split into chunks"
    (let [text (apply str (repeat 500 "word "))
          chunks (ingestion/chunk-text text :chunk-size 100 :overlap 20)]
      (is (> (count chunks) 1))
      (is (every? #(<= (count (:text %)) 100) chunks))
      (is (every? #(contains? % :start) chunks))
      (is (every? #(contains? % :end) chunks)))))

(deftest test-chunk-text-overlap
  (testing "Chunks have proper overlap"
    (let [text (apply str (repeat 100 "a"))
          chunks (ingestion/chunk-text text :chunk-size 30 :overlap 10)]
      (is (> (count chunks) 1))
      (when (>= (count chunks) 2)
        (let [first-end (:end (first chunks))
              second-start (:start (second chunks))]
          (is (< second-start first-end)))))))

(deftest test-chunk-text-nil
  (testing "Handle nil text"
    (let [chunks (ingestion/chunk-text nil)]
      (is (= 1 (count chunks)))
      (is (nil? (:text (first chunks)))))))

(deftest test-chunk-text-empty
  (testing "Handle empty text"
    (let [chunks (ingestion/chunk-text "")]
      (is (= 1 (count chunks)))
      (is (= "" (:text (first chunks)))))))

(deftest test-chunk-by-sentences
  (testing "Chunk by sentence boundaries"
    (let [text "First sentence. Second sentence. Third sentence."
          chunks (ingestion/chunk-by-sentences text :max-chunk-size 100)]
      (is (seq chunks))
      (is (every? #(contains? % :text) chunks))
      (is (every? #(contains? % :sentences) chunks)))))

(deftest test-chunk-by-paragraphs
  (testing "Chunk by paragraph boundaries"
    (let [text "First paragraph.\n\nSecond paragraph.\n\nThird paragraph."
          chunks (ingestion/chunk-by-paragraphs text)]
      (is (= 3 (count chunks)))
      (is (every? #(= :paragraph (:type %)) chunks))
      (is (every? #(contains? % :index) chunks)))))

;; ============================================
;; File Processing Tests
;; ============================================

(deftest test-get-file-extension
  (testing "Get file extension"
    (is (= ".txt" (ingestion/get-file-extension "/path/to/file.txt")))
    (is (= ".pdf" (ingestion/get-file-extension "/path/to/file.pdf")))
    (is (= ".TXT" (ingestion/get-file-extension "/path/to/FILE.TXT")))
    (is (= "" (ingestion/get-file-extension "/path/to/noextension")))))

(deftest test-is-supported-file
  (testing "Check supported file extensions"
    (is (true? (ingestion/is-supported-file? "/path/to/file.txt")))
    (is (true? (ingestion/is-supported-file? "/path/to/file.md")))
    (is (true? (ingestion/is-supported-file? "/path/to/file.json")))
    (is (false? (ingestion/is-supported-file? "/path/to/file.exe")))
    (is (false? (ingestion/is-supported-file? "/path/to/file.xyz")))))

(deftest test-read-text-file-success
  (testing "Read existing text file"
    (let [test-file "/tmp/ingestion-test.txt"
          _ (spit test-file "Test content")
          result (ingestion/read-text-file test-file)]
      (is (true? (:success result)))
      (is (= "Test content" (:content result)))
      (is (= test-file (:path result)))
      (is (= ".txt" (:extension result)))
      (io/delete-file test-file))))

(deftest test-read-text-file-failure
  (testing "Read non-existent file"
    (let [result (ingestion/read-text-file "/nonexistent/file.txt")]
      (is (false? (:success result)))
      (is (contains? result :error)))))

(deftest test-process-text-file
  (testing "Process text file into chunks"
    (let [test-file "/tmp/process-test.txt"
          content (apply str (repeat 500 "word "))
          _ (spit test-file content)
          result (ingestion/process-text-file test-file :chunk-size 100)]
      (is (true? (:success result)))
      (is (> (:total-chunks result) 1))
      (is (= (count content) (:total-characters result)))
      (io/delete-file test-file))))

;; ============================================
;; PDF Processing Tests
;; ============================================

(deftest test-process-pdf-stub
  (testing "PDF processing returns stub"
    (let [result (ingestion/process-pdf "/path/to/file.pdf")]
      (is (false? (:success result)))
      (is (contains? result :note))
      (is (contains? result :alternative)))))

;; ============================================
;; Batch Processing Tests
;; ============================================

(deftest test-list-files-recursive
  (testing "List files in directory"
    (let [test-dir "/tmp/ingestion-batch-test"
          _ (.mkdirs (io/file test-dir))
          _ (spit (str test-dir "/file1.txt") "content1")
          _ (spit (str test-dir "/file2.txt") "content2")
          files (ingestion/list-files-recursive test-dir)]
      (is (= 2 (count files)))
      (is (every? #(clojure.string/ends-with? % ".txt") files))
      (io/delete-file (str test-dir "/file1.txt"))
      (io/delete-file (str test-dir "/file2.txt"))
      (.delete (io/file test-dir)))))

(deftest test-batch-process-directory
  (testing "Batch process directory"
    (let [test-dir "/tmp/ingestion-batch-test2"
          _ (.mkdirs (io/file test-dir))
          _ (spit (str test-dir "/file1.txt") "content1")
          _ (spit (str test-dir "/file2.md") "content2")
          result (ingestion/batch-process-directory test-dir)]
      (is (map? result))
      (is (= test-dir (:directory result)))
      (is (= 2 (:supported-files result)))
      (is (= 2 (:processed result)))
      (io/delete-file (str test-dir "/file1.txt"))
      (io/delete-file (str test-dir "/file2.md"))
      (.delete (io/file test-dir)))))

;; ============================================
;; File Watching Tests
;; ============================================

(deftest test-start-stop-watching
  (testing "Start and stop file watching"
    (let [start-result (ingestion/start-watching "/tmp" (fn [_] nil))]
      (is (= :started (:status start-result)))
      (is (true? (:watching (ingestion/get-watch-status))))
      (let [stop-result (ingestion/stop-watching)]
        (is (= :stopped (:status stop-result)))
        (is (false? (:watching (ingestion/get-watch-status))))))))

(deftest test-get-watch-status
  (testing "Get watch status"
    (ingestion/stop-watching)
    (let [status (ingestion/get-watch-status)]
      (is (map? status))
      (is (contains? status :watching))
      (is (contains? status :files)))))

;; ============================================
;; Feedback Collection Tests
;; ============================================

(deftest test-submit-feedback
  (testing "Submit feedback"
    (let [feedback (ingestion/submit-feedback "doc-123" :correction "Fixed typo" :user-id "user-1")]
      (is (map? feedback))
      (is (contains? feedback :id))
      (is (= "doc-123" (:doc-id feedback)))
      (is (= :correction (:type feedback)))
      (is (= "Fixed typo" (:content feedback)))
      (is (= "user-1" (:user-id feedback)))
      (is (contains? feedback :timestamp)))))

(deftest test-get-feedback
  (testing "Get feedback for document"
    (reset! ingestion/feedback-store [])
    (ingestion/submit-feedback "doc-456" :comment "Good analysis")
    (ingestion/submit-feedback "doc-456" :suggestion "Add more detail")
    (ingestion/submit-feedback "doc-789" :comment "Different doc")
    (let [feedback (ingestion/get-feedback "doc-456")]
      (is (= 2 (count feedback)))
      (is (every? #(= "doc-456" (:doc-id %)) feedback)))))

(deftest test-get-all-feedback
  (testing "Get all feedback"
    (reset! ingestion/feedback-store [])
    (ingestion/submit-feedback "doc-1" :comment "Comment 1")
    (ingestion/submit-feedback "doc-2" :comment "Comment 2")
    (let [all-feedback (ingestion/get-all-feedback)]
      (is (= 2 (count all-feedback))))))

;; ============================================
;; Pipeline Tests
;; ============================================

(deftest test-create-pipeline
  (testing "Create pipeline"
    (let [pipeline (ingestion/create-pipeline :chunk-size 500 :overlap 100)]
      (is (map? pipeline))
      (is (= 500 (:chunk-size pipeline)))
      (is (= 100 (:overlap pipeline)))
      (is (= :ready (:status pipeline))))))

(deftest test-run-pipeline
  (testing "Run pipeline"
    (let [pipeline (ingestion/create-pipeline :chunk-size 50 :overlap 10)
          input (apply str (repeat 100 "word "))
          result (ingestion/run-pipeline pipeline input)]
      (is (map? result))
      (is (= (count input) (:input-length result)))
      (is (> (:total-chunks result) 1))
      (is (= :completed (:pipeline-status result))))))

(deftest test-run-pipeline-with-processors
  (testing "Run pipeline with processors"
    (let [upper-case-processor (fn [chunk] (update chunk :text clojure.string/upper-case))
          pipeline (ingestion/create-pipeline :chunk-size 100 :processors [upper-case-processor])
          result (ingestion/run-pipeline pipeline "test input")]
      (is (= :completed (:pipeline-status result)))
      (is (every? #(= (:text %) (clojure.string/upper-case (:text %))) (:chunks result))))))

;; ============================================
;; Large File Processing Tests
;; ============================================

(deftest test-get-processing-stats
  (testing "Get processing stats"
    (ingestion/reset-processing-stats)
    (let [stats (ingestion/get-processing-stats)]
      (is (map? stats))
      (is (= 0 (:total-processed stats)))
      (is (= 0 (:total-bytes stats))))))

(deftest test-reset-processing-stats
  (testing "Reset processing stats"
    (swap! ingestion/processing-state assoc :total-processed 100)
    (ingestion/reset-processing-stats)
    (is (= 0 (:total-processed (ingestion/get-processing-stats))))))

(deftest test-process-large-file
  (testing "Process large file"
    (let [test-file "/tmp/large-file-test.txt"
          content (clojure.string/join "\n" (repeat 100 "Line of text"))
          _ (spit test-file content)
          _ (ingestion/reset-processing-stats)
          result (ingestion/process-large-file test-file :batch-size 10)]
      (is (true? (:success result)))
      (is (> (:total-processed result) 0))
      (io/delete-file test-file))))

;; ============================================
;; Data Validation Tests
;; ============================================

(deftest test-validate-chunk-valid
  (testing "Validate valid chunk"
    (let [chunk {:text "Valid text content" :start 0 :end 18}
          result (ingestion/validate-chunk chunk)]
      (is (true? (:valid result)))
      (is (empty? (:issues result))))))

(deftest test-validate-chunk-nil-text
  (testing "Validate chunk with nil text"
    (let [chunk {:text nil :start 0 :end 0}
          result (ingestion/validate-chunk chunk)]
      (is (false? (:valid result)))
      (is (some #(clojure.string/includes? % "nil") (:issues result))))))

(deftest test-validate-chunk-empty-text
  (testing "Validate chunk with empty text"
    (let [chunk {:text "   " :start 0 :end 3}
          result (ingestion/validate-chunk chunk)]
      (is (false? (:valid result)))
      (is (some #(clojure.string/includes? % "empty") (:issues result))))))

(deftest test-validate-document
  (testing "Validate document"
    (let [doc {:chunks [{:text "Valid chunk 1"}
                        {:text "Valid chunk 2"}
                        {:text nil}]}
          result (ingestion/validate-document doc)]
      (is (map? result))
      (is (= 3 (:total-chunks result)))
      (is (= 2 (:valid-chunks result)))
      (is (= 1 (:invalid-chunks result)))
      (is (false? (:valid result))))))

;; ============================================
;; Edge Cases
;; ============================================

(deftest test-chunk-exact-size
  (testing "Text exactly chunk size"
    (let [text (apply str (repeat 100 "a"))
          chunks (ingestion/chunk-text text :chunk-size 100 :overlap 20)]
      (is (= 1 (count chunks))))))

(deftest test-unicode-handling
  (testing "Handle unicode text"
    (let [text "Hello"
          chunks (ingestion/chunk-text text)]
      (is (= 1 (count chunks)))
      (is (= text (:text (first chunks)))))))

(deftest test-special-characters
  (testing "Handle special characters"
    (let [text "Line 1\nLine 2\tTabbed\r\nWindows line"
          chunks (ingestion/chunk-text text)]
      (is (seq chunks))
      (is (= text (:text (first chunks)))))))
