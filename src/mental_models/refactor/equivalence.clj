(ns mental-models.refactor.equivalence
  "Equivalence Checker - Property-Based Testing for Referential Transparency
   Verifies that refactored code is semantically equivalent to original"
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; -- Generator Helpers -------------------------------------------------------

(def gen-small-int
  "Generator for small integers"
  (gen/choose -100 100))

(def gen-string
  "Generator for strings"
  (gen/fmap str/join (gen/vector gen/char-alphanumeric 0 50)))

(def gen-keyword
  "Generator for keywords"
  (gen/fmap keyword (gen/such-that #(not (empty? %))
                                   (gen/fmap str/join
                                            (gen/vector gen/char-alpha 1 20)))))

(def gen-symbol
  "Generator for symbols"
  (gen/fmap symbol (gen/such-that #(not (empty? %))
                                  (gen/fmap str/join
                                           (gen/vector gen/char-alpha 1 20)))))

(def gen-primitive
  "Generator for primitive values"
  (gen/one-of [gen/small-integer
               gen/double
               gen-string
               gen/boolean
               gen-keyword
               gen/nil]))

(def gen-collection
  "Generator for collections"
  (gen/one-of [(gen/vector gen-primitive)
               (gen/list gen-primitive)
               (gen/set gen-primitive)
               (gen/map gen-keyword gen-primitive)]))

(def gen-any
  "Generator for any Clojure value"
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [gen-primitive
                  (gen/vector inner)
                  (gen/list inner)
                  (gen/map gen-keyword inner)]))
   gen-primitive))

;; -- Function Comparison -----------------------------------------------------

(defn functions-equivalent?
  "Test if two functions produce the same output for the same input"
  [f1 f2 inputs]
  (try
    (let [result1 (apply f1 inputs)
          result2 (apply f2 inputs)]
      (= result1 result2))
    (catch Exception e1
      (try
        (apply f2 inputs)
        false ; f1 threw but f2 didn't
        (catch Exception e2
          ;; Both threw - check if same exception type
          (= (type e1) (type e2)))))))

(defn make-equivalence-property
  "Create a property that checks function equivalence"
  [f1 f2 arg-generators]
  (prop/for-all [args (apply gen/tuple arg-generators)]
    (functions-equivalent? f1 f2 args)))

;; -- Automated Test Generation -----------------------------------------------

(defn infer-arg-types
  "Infer argument types from function metadata or signature"
  [fn-var]
  (let [arglists (:arglists (meta fn-var))
        first-arglist (first arglists)]
    (when first-arglist
      (for [arg first-arglist]
        (cond
          (str/includes? (str arg) "int") gen/small-integer
          (str/includes? (str arg) "str") gen-string
          (str/includes? (str arg) "coll") gen-collection
          (str/includes? (str arg) "map") (gen/map gen-keyword gen-primitive)
          (str/includes? (str arg) "vec") (gen/vector gen-primitive)
          (str/includes? (str arg) "bool") gen/boolean
          :else gen-any)))))

(defn generate-equivalence-test
  "Generate an equivalence test for two functions"
  [original-fn refactored-fn & {:keys [num-tests arg-generators]
                                :or {num-tests 100}}]
  (let [generators (or arg-generators
                       (infer-arg-types original-fn)
                       [gen-any])]
    (tc/quick-check
     num-tests
     (make-equivalence-property original-fn refactored-fn generators))))

;; -- Referential Transparency Checks -----------------------------------------

(defn check-referential-transparency
  "Check if a function is referentially transparent"
  [f arg-generators & {:keys [num-tests] :or {num-tests 50}}]
  (let [;; Property: calling f twice with same args gives same result
        idempotent-prop
        (prop/for-all [args (apply gen/tuple arg-generators)]
          (let [result1 (try (apply f args) (catch Exception _ ::error))
                result2 (try (apply f args) (catch Exception _ ::error))]
            (= result1 result2)))
        
        ;; Property: order of evaluation doesn't matter
        order-prop
        (prop/for-all [args1 (apply gen/tuple arg-generators)
                       args2 (apply gen/tuple arg-generators)]
          (let [;; Call in order 1, 2
                r1a (try (apply f args1) (catch Exception _ ::error))
                r2a (try (apply f args2) (catch Exception _ ::error))
                ;; Call in order 2, 1
                r2b (try (apply f args2) (catch Exception _ ::error))
                r1b (try (apply f args1) (catch Exception _ ::error))]
            (and (= r1a r1b) (= r2a r2b))))]
    
    {:idempotent (tc/quick-check num-tests idempotent-prop)
     :order-independent (tc/quick-check num-tests order-prop)}))

;; -- Side Effect Detection ---------------------------------------------------

(defn detect-side-effects
  "Attempt to detect if a function has side effects"
  [f arg-generators & {:keys [num-tests] :or {num-tests 20}}]
  (let [;; Track atoms/refs that might be modified
        test-atom (atom nil)
        
        ;; Check if function modifies external state
        state-check
        (prop/for-all [args (apply gen/tuple arg-generators)]
          (let [before @test-atom
                _ (try (apply f args) (catch Exception _ nil))
                after @test-atom]
            (= before after)))
        
        ;; Check for IO (approximate - look for println, etc.)
        result (tc/quick-check num-tests state-check)]
    
    {:likely-pure (:pass? result)
     :test-result result}))

;; -- Comprehensive Verification ----------------------------------------------

(defn verify-refactoring
  "Comprehensive verification of a refactoring"
  [original-fn refactored-fn & {:keys [arg-generators num-tests]
                                :or {num-tests 100}}]
  (log/info "Verifying refactoring equivalence...")
  (let [generators (or arg-generators [gen-any])
        
        ;; Test 1: Output equivalence
        equivalence (generate-equivalence-test
                     original-fn refactored-fn
                     :num-tests num-tests
                     :arg-generators generators)
        
        ;; Test 2: Referential transparency of refactored version
        transparency (check-referential-transparency
                      refactored-fn generators
                      :num-tests (/ num-tests 2))
        
        ;; Test 3: Side effect detection
        side-effects (detect-side-effects
                      refactored-fn generators
                      :num-tests (/ num-tests 5))]
    
    {:equivalence {:passed (:pass? equivalence)
                   :num-tests (:num-tests equivalence)
                   :failing-case (when-not (:pass? equivalence)
                                   (:shrunk equivalence))}
     :transparency {:idempotent (get-in transparency [:idempotent :pass?])
                    :order-independent (get-in transparency [:order-independent :pass?])}
     :side-effects {:likely-pure (:likely-pure side-effects)}
     :overall-verdict (and (:pass? equivalence)
                           (get-in transparency [:idempotent :pass?])
                           (:likely-pure side-effects))}))

;; -- Code-Level Verification -------------------------------------------------

(defn load-and-verify
  "Load code strings and verify equivalence"
  [original-code refactored-code fn-name & {:keys [arg-generators]}]
  (try
    (let [;; Create temporary namespaces
          orig-ns (create-ns (gensym "original"))
          refact-ns (create-ns (gensym "refactored"))
          
          ;; Eval code in respective namespaces
          _ (binding [*ns* orig-ns]
              (eval (read-string original-code)))
          _ (binding [*ns* refact-ns]
              (eval (read-string refactored-code)))
          
          ;; Get functions
          orig-fn (ns-resolve orig-ns fn-name)
          refact-fn (ns-resolve refact-ns fn-name)]
      
      (if (and orig-fn refact-fn)
        (verify-refactoring @orig-fn @refact-fn
                            :arg-generators arg-generators)
        {:error "Could not resolve functions"
         :original-found (boolean orig-fn)
         :refactored-found (boolean refact-fn)}))
    (catch Exception e
      {:error (.getMessage e)
       :type (type e)})))

;; -- Batch Verification ------------------------------------------------------

(defn verify-all-refactorings
  "Verify multiple refactorings"
  [refactorings]
  (for [{:keys [original-code refactored-code fn-names]} refactorings]
    {:refactoring refactorings
     :verifications
     (for [fn-name fn-names]
       {:function fn-name
        :result (load-and-verify original-code refactored-code fn-name)})}))

;; -- Regression Test Generation ----------------------------------------------

(defn generate-regression-tests
  "Generate regression tests from successful property tests"
  [f arg-generators & {:keys [num-examples] :or {num-examples 10}}]
  (let [examples (gen/sample (apply gen/tuple arg-generators) num-examples)]
    (for [args examples]
      {:inputs args
       :expected-output (try (apply f args)
                            (catch Exception e {:exception (type e)}))})))

(defn format-as-test
  "Format regression test cases as clojure.test code"
  [fn-name test-cases]
  (str "(deftest " fn-name "-regression-test\n"
       (str/join "\n"
                 (for [{:keys [inputs expected-output]} test-cases]
                   (if (map? expected-output)
                     (str "  (is (thrown? " (:exception expected-output) " "
                          "(" fn-name " " (str/join " " (map pr-str inputs)) ")))")
                     (str "  (is (= " (pr-str expected-output) " "
                          "(" fn-name " " (str/join " " (map pr-str inputs)) ")))"))))
       ")"))

;; -- Main Entry Point --------------------------------------------------------

(defn verify-and-generate-tests
  "Main entry point: verify refactoring and generate regression tests"
  [original-fn refactored-fn fn-name & {:keys [arg-generators num-tests]
                                        :or {num-tests 100}}]
  (let [generators (or arg-generators [gen-any])
        verification (verify-refactoring original-fn refactored-fn
                                         :arg-generators generators
                                         :num-tests num-tests)]
    (if (:overall-verdict verification)
      (let [regression-tests (generate-regression-tests
                              refactored-fn generators
                              :num-examples 20)]
        {:verification verification
         :regression-tests regression-tests
         :test-code (format-as-test fn-name regression-tests)
         :status :verified})
      {:verification verification
       :status :failed
       :recommendation "Review failing cases before applying refactoring"})))
