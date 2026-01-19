(ns mental-models.code-quality-test
  "Comprehensive code quality tests to prevent regressions.
   
   These tests catch common Clojure bugs BEFORE they reach production:
   - Variable shadowing (e.g., naming a var 'count' shadows clojure.core/count)
   - Invalid regex escape sequences
   - Static field reference errors
   - Type casting issues
   - Namespace resolution problems
   
   Run with: clj -M:test"
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; =============================================================================
;; Test Configuration
;; =============================================================================

(def source-files
  "All Clojure source files that should be tested"
  ["src/mental_models/desktop/gui/swing_app.clj"
   "src/mental_models/desktop/updater/github_checker.clj"
   "src/mental_models/desktop/updater/blue_green.clj"
   "src/mental_models/desktop/updater/continuous_deploy.clj"
   "src/mental_models/desktop/updater/hot_reload.clj"])

;; =============================================================================
;; Variable Shadowing Detection
;; =============================================================================

(def clojure-core-functions
  "Common clojure.core functions that should NEVER be shadowed"
  #{"count" "first" "rest" "next" "cons" "conj" "assoc" "dissoc" "get" "set"
    "map" "filter" "reduce" "apply" "str" "name" "keyword" "symbol"
    "inc" "dec" "+" "-" "*" "/" "mod" "rem" "quot"
    "=" "==" "not=" "<" ">" "<=" ">=" "compare"
    "and" "or" "not" "true?" "false?" "nil?" "some?" "empty?"
    "list" "vector" "hash-map" "hash-set" "sorted-map" "sorted-set"
    "seq" "sequence" "lazy-seq" "doall" "dorun"
    "atom" "ref" "agent" "deref" "reset!" "swap!" "alter" "send"
    "fn" "defn" "let" "loop" "recur" "if" "when" "cond" "case"
    "try" "catch" "finally" "throw"
    "print" "println" "pr" "prn" "format"
    "read" "read-string" "slurp" "spit"
    "type" "class" "instance?" "isa?"
    "meta" "with-meta" "vary-meta"
    "keys" "vals" "key" "val" "find" "contains?" "select-keys"
    "concat" "flatten" "distinct" "sort" "sort-by" "reverse"
    "take" "drop" "take-while" "drop-while" "split-at" "split-with"
    "partition" "partition-by" "partition-all" "group-by"
    "interleave" "interpose" "zipmap"
    "range" "repeat" "repeatedly" "iterate" "cycle"
    "identity" "constantly" "comp" "partial" "juxt" "complement"
    "every?" "some" "not-every?" "not-any?"
    "max" "min" "max-key" "min-key"
    "rand" "rand-int" "rand-nth" "shuffle"
    "time" "future" "promise" "deliver" "realized?"})

(defn find-variable-shadowing
  "Find let bindings that shadow clojure.core functions"
  [source-code]
  (let [;; Match let bindings like (let [count ...] or [count (something)]
        let-binding-pattern #"\[\s*(\w+)\s+(?:\(|[^\]\s])"
        matches (re-seq let-binding-pattern source-code)]
    (->> matches
         (map second)
         (filter clojure-core-functions)
         (distinct))))

(deftest test-no-variable-shadowing
  (testing "No variables should shadow clojure.core functions"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)
                shadows (find-variable-shadowing source)]
            (is (empty? shadows)
                (str "File " file " shadows clojure.core functions: " 
                     (str/join ", " shadows)
                     "\n\nThis causes 'Integer cannot be cast to IFn' errors!"
                     "\nRename these variables to avoid shadowing."))))))))

;; =============================================================================
;; Regex Escape Sequence Validation
;; =============================================================================

(defn find-invalid-regex-escapes
  "Find potentially invalid regex escape sequences in re-pattern calls"
  [source-code]
  (let [;; Find re-pattern calls with string concatenation
        pattern #"\(re-pattern\s+\(str[^)]+\"[^\"]*\\[^\\\"nrtfbdwsDWsSbB0-9xuU\[\](){}|^$.*+?-][^\"]*\""
        matches (re-seq pattern source-code)]
    matches))

(defn find-raw-regex-issues
  "Find regex literals with potential issues"
  [source-code]
  (let [;; Find #"..." patterns and check for common issues
        lines (str/split-lines source-code)
        issues (atom [])]
    (doseq [[idx line] (map-indexed vector lines)]
      ;; Check for unescaped special chars in dynamic regex
      (when (and (str/includes? line "re-pattern")
                 (str/includes? line "(str")
                 (or (str/includes? line "\\s*")
                     (str/includes? line "\\d*")
                     (str/includes? line "\\w*")))
        ;; These need double escaping in str: \\\\s not \\s
        (when-not (or (str/includes? line "\\\\s")
                      (str/includes? line "\\\\d")
                      (str/includes? line "\\\\w"))
          (swap! issues conj {:line (inc idx) :content line}))))
    @issues))

(deftest test-regex-escape-sequences
  (testing "Regex escape sequences should be properly escaped"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)
                issues (find-raw-regex-issues source)]
            (is (empty? issues)
                (str "File " file " has regex escape issues:\n"
                     (str/join "\n" (map #(str "  Line " (:line %) ": " (:content %)) issues))
                     "\n\nIn (re-pattern (str ...)), use \\\\s not \\s"))))))))

;; =============================================================================
;; Static Field Reference Validation
;; =============================================================================

(def java-static-fields
  "Common Java static fields that should NOT have parens"
  #{"ISO_LOCAL_DATE_TIME" "ISO_LOCAL_DATE" "ISO_LOCAL_TIME"
    "ISO_OFFSET_DATE_TIME" "ISO_ZONED_DATE_TIME" "ISO_INSTANT"
    "BASIC_ISO_DATE" "ISO_DATE" "ISO_TIME" "ISO_DATE_TIME"
    "RFC_1123_DATE_TIME" "ISO_WEEK_DATE" "ISO_ORDINAL_DATE"
    "BOLD" "PLAIN" "ITALIC"
    "CENTER" "LEFT" "RIGHT" "TOP" "BOTTOM"
    "BLACK" "WHITE" "RED" "GREEN" "BLUE"
    "MAX_VALUE" "MIN_VALUE"
    "PI" "E"
    "NORTH" "SOUTH" "EAST" "WEST"
    "HORIZONTAL" "VERTICAL"})

(defn find-static-field-calls
  "Find static fields being called as functions (with parens)"
  [source-code]
  (let [pattern (re-pattern (str "\\((" (str/join "|" java-static-fields) ")\\)"))
        matches (re-seq pattern source-code)]
    (map first matches)))

(deftest test-static-field-references
  (testing "Static fields should not be called as functions"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)
                issues (find-static-field-calls source)]
            (is (empty? issues)
                (str "File " file " calls static fields as functions: "
                     (str/join ", " issues)
                     "\n\nUse DateTimeFormatter/ISO_LOCAL_DATE_TIME not (DateTimeFormatter/ISO_LOCAL_DATE_TIME)"))))))))

;; =============================================================================
;; Parenthesis Balance Check
;; =============================================================================

(defn check-paren-balance
  "Check if parentheses, brackets, and braces are balanced"
  [source-code]
  (let [opens {\( \) \[ \] \{ \}}
        closes (set (vals opens))
        stack (atom [])]
    (doseq [ch source-code]
      (cond
        (contains? opens ch) (swap! stack conj ch)
        (contains? closes ch) 
        (let [expected (get opens (peek @stack))]
          (if (= ch expected)
            (swap! stack pop)
            (throw (ex-info "Mismatched delimiter" {:expected expected :got ch}))))))
    (empty? @stack)))

(deftest test-balanced-delimiters
  (testing "All delimiters should be balanced"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)
                ;; Remove strings and comments for accurate check
                cleaned (-> source
                           (str/replace #"\"(?:[^\"\\]|\\.)*\"" "\"\"")
                           (str/replace #";[^\n]*" ""))]
            (is (check-paren-balance cleaned)
                (str "File " file " has unbalanced delimiters"))))))))

;; =============================================================================
;; Integer-as-Function Detection
;; =============================================================================

(defn find-number-function-calls
  "Find patterns where a number might be called as a function"
  [source-code]
  (let [;; Pattern: (123 something) - number in function position
        pattern #"\(\s*\d+\s+[^\)]+\)"
        matches (re-seq pattern source-code)]
    matches))

(deftest test-no-number-function-calls
  (testing "Numbers should not be in function position"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)
                issues (find-number-function-calls source)]
            (is (empty? issues)
                (str "File " file " has numbers in function position: "
                     (str/join ", " issues)))))))))

;; =============================================================================
;; Common Bug Pattern Detection
;; =============================================================================

(def dangerous-patterns
  "Patterns that often indicate bugs"
  [{:name "Shadowed count variable"
    :pattern #"\(let\s+\[\s*count\s+"
    :message "Variable 'count' shadows clojure.core/count - rename to 'item-count' or similar"}
   {:name "Shadowed map variable"
    :pattern #"\(let\s+\[\s*map\s+"
    :message "Variable 'map' shadows clojure.core/map - rename to 'data-map' or similar"}
   {:name "Shadowed filter variable"
    :pattern #"\(let\s+\[\s*filter\s+"
    :message "Variable 'filter' shadows clojure.core/filter - rename to 'filter-fn' or similar"}
   {:name "Unescaped regex in re-pattern str"
    :pattern #"\(re-pattern\s+\(str[^)]+\"[^\"]*\\s\*"
    :message "Use \\\\s* not \\s* inside (re-pattern (str ...))"}
   {:name "Static field with parens"
    :pattern #"\(DateTimeFormatter/ISO_LOCAL_DATE_TIME\)"
    :message "Remove parens: DateTimeFormatter/ISO_LOCAL_DATE_TIME not (DateTimeFormatter/ISO_LOCAL_DATE_TIME)"}])

(deftest test-dangerous-patterns
  (testing "No dangerous code patterns should exist"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)]
            (doseq [{:keys [name pattern message]} dangerous-patterns]
              (is (nil? (re-find pattern source))
                  (str "File " file " contains dangerous pattern: " name
                       "\n" message)))))))))

;; =============================================================================
;; Namespace Validation
;; =============================================================================

(deftest test-namespace-declaration
  (testing "All files should have valid namespace declarations"
    (doseq [file source-files]
      (let [full-path (str "desktop/" file)]
        (when (.exists (io/file full-path))
          (let [source (slurp full-path)]
            (is (re-find #"^\s*\(ns\s+" source)
                (str "File " file " missing namespace declaration"))))))))

;; =============================================================================
;; Run All Tests
;; =============================================================================

(defn run-all-tests []
  "Run all code quality tests and return results"
  (run-tests 'mental-models.code-quality-test))

(comment
  ;; Run tests from REPL
  (run-all-tests))
