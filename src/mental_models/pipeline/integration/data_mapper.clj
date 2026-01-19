(ns mental-models.pipeline.integration.data-mapper
  "Data Mapper Module
   
   Object-relational mapping and data transformation:
   - Schema mapping definitions
   - Field transformations
   - Nested object mapping
   - Collection mapping
   - Bidirectional mapping"
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; DATA MAPPER STATE
;; =============================================================================

(defonce mapper-state (atom {:mappings (ConcurrentHashMap.)
                             :transformers (ConcurrentHashMap.)
                             :converters {}
                             :mapping-count (AtomicLong. 0)
                             :config {:strict-mode false
                                      :null-handling :keep
                                      :unknown-fields :ignore}}))

;; =============================================================================
;; FIELD TRANSFORMERS
;; =============================================================================

(defn register-transformer!
  "Register a field transformer."
  [transformer-id transform-fn & {:keys [description]}]
  (log/info "Registering transformer" {:id transformer-id})
  (.put ^ConcurrentHashMap (:transformers @mapper-state) transformer-id
        {:fn transform-fn
         :description description}))

(defn unregister-transformer!
  "Unregister a transformer."
  [transformer-id]
  (.remove ^ConcurrentHashMap (:transformers @mapper-state) transformer-id))

(defn get-transformer
  "Get a transformer by ID."
  [transformer-id]
  (.get ^ConcurrentHashMap (:transformers @mapper-state) transformer-id))

(defn apply-transformer
  "Apply a transformer to a value."
  [transformer-id value]
  (if-let [transformer (get-transformer transformer-id)]
    ((:fn transformer) value)
    value))

;; =============================================================================
;; TYPE CONVERTERS
;; =============================================================================

(defn register-converter!
  "Register a type converter."
  [from-type to-type convert-fn]
  (swap! mapper-state assoc-in [:converters [from-type to-type]] convert-fn))

(defn get-converter
  "Get a converter for types."
  [from-type to-type]
  (get-in @mapper-state [:converters [from-type to-type]]))

(defn convert-type
  "Convert a value from one type to another."
  [value from-type to-type]
  (if-let [converter (get-converter from-type to-type)]
    (converter value)
    value))

;; =============================================================================
;; MAPPING DEFINITION
;; =============================================================================

(defn create-field-mapping
  "Create a field mapping definition."
  [{:keys [source target transformer converter default required rename]}]
  {:source source
   :target (or target source)
   :transformer transformer
   :converter converter
   :default default
   :required (boolean required)
   :rename rename})

(defn create-mapping
  "Create a mapping definition."
  [mapping-id {:keys [source-type target-type fields nested description]}]
  {:id mapping-id
   :source-type source-type
   :target-type target-type
   :fields (into {} (map (fn [f] [(:source f) (create-field-mapping f)]) fields))
   :nested (or nested {})
   :description description
   :created-at (System/currentTimeMillis)})

(defn register-mapping!
  "Register a data mapping."
  [mapping-id fields & {:keys [source-type target-type nested description]}]
  (log/info "Registering mapping" {:id mapping-id})
  (let [mapping (create-mapping mapping-id {:source-type source-type
                                            :target-type target-type
                                            :fields fields
                                            :nested nested
                                            :description description})]
    (.put ^ConcurrentHashMap (:mappings @mapper-state) mapping-id mapping)
    (metrics/inc-counter! :datamapper/mappings-registered)
    (events/publish! :datamapper/mapping-registered {:mapping-id mapping-id})
    mapping-id))

(defn unregister-mapping!
  "Unregister a mapping."
  [mapping-id]
  (log/info "Unregistering mapping" {:id mapping-id})
  (.remove ^ConcurrentHashMap (:mappings @mapper-state) mapping-id))

(defn get-mapping
  "Get a mapping by ID."
  [mapping-id]
  (.get ^ConcurrentHashMap (:mappings @mapper-state) mapping-id))

(defn list-mappings
  "List all registered mappings."
  []
  (vec (keys (:mappings @mapper-state))))

;; =============================================================================
;; MAPPING EXECUTION
;; =============================================================================

(defn get-nested-value
  "Get a nested value from a map using a path."
  [data path]
  (if (vector? path)
    (get-in data path)
    (get data path)))

(defn set-nested-value
  "Set a nested value in a map using a path."
  [data path value]
  (if (vector? path)
    (assoc-in data path value)
    (assoc data path value)))

(defn map-field
  "Map a single field."
  [source-data field-mapping]
  (let [{:keys [source target transformer converter default required]} field-mapping
        source-value (get-nested-value source-data source)
        value (cond
                (some? source-value) source-value
                (some? default) default
                :else nil)]
    (when (and required (nil? value))
      (throw (ex-info "Required field missing" {:field source})))
    (let [transformed (if (and transformer (some? value))
                        (apply-transformer transformer value)
                        value)
          converted (if (and converter (some? transformed))
                      (let [[from-type to-type] converter]
                        (convert-type transformed from-type to-type))
                      transformed)]
      [target converted])))

(defn map-object
  "Map an object using a mapping definition."
  [source-data mapping-id]
  (.incrementAndGet ^AtomicLong (:mapping-count @mapper-state))
  (metrics/inc-counter! :datamapper/objects-mapped)
  (if-let [mapping (get-mapping mapping-id)]
    (let [null-handling (get-in @mapper-state [:config :null-handling])
          unknown-handling (get-in @mapper-state [:config :unknown-fields])
          ;; Map defined fields
          mapped-fields (into {} (for [[_ field-mapping] (:fields mapping)
                                       :let [[target value] (map-field source-data field-mapping)]
                                       :when (or (some? value) (= null-handling :keep))]
                                   [target value]))
          ;; Handle nested mappings
          with-nested (reduce (fn [result [field nested-mapping-id]]
                                (if-let [nested-data (get source-data field)]
                                  (assoc result field
                                         (if (sequential? nested-data)
                                           (mapv #(map-object % nested-mapping-id) nested-data)
                                           (map-object nested-data nested-mapping-id)))
                                  result))
                              mapped-fields
                              (:nested mapping))
          ;; Handle unknown fields
          result (if (= unknown-handling :include)
                   (merge (apply dissoc source-data (keys (:fields mapping))) with-nested)
                   with-nested)]
      result)
    (do
      (log/warn "Mapping not found" {:mapping-id mapping-id})
      source-data)))

(defn map-collection
  "Map a collection of objects."
  [source-collection mapping-id]
  (mapv #(map-object % mapping-id) source-collection))

;; =============================================================================
;; REVERSE MAPPING
;; =============================================================================

(defn create-reverse-mapping
  "Create a reverse mapping from an existing mapping."
  [mapping-id reverse-mapping-id]
  (when-let [mapping (get-mapping mapping-id)]
    (let [reverse-fields (for [[source field-mapping] (:fields mapping)]
                           {:source (:target field-mapping)
                            :target source
                            :transformer (:transformer field-mapping)
                            :converter (when-let [[from to] (:converter field-mapping)]
                                         [to from])
                            :default (:default field-mapping)
                            :required (:required field-mapping)})]
      (register-mapping! reverse-mapping-id reverse-fields
                         :source-type (:target-type mapping)
                         :target-type (:source-type mapping)
                         :description (str "Reverse of " mapping-id)))))

(defn map-reverse
  "Map an object using the reverse of a mapping."
  [target-data mapping-id]
  (let [reverse-id (keyword (str (name mapping-id) "-reverse"))]
    (when-not (get-mapping reverse-id)
      (create-reverse-mapping mapping-id reverse-id))
    (map-object target-data reverse-id)))

;; =============================================================================
;; MAPPING DSL
;; =============================================================================

(defmacro defmapping
  "Define a mapping using a DSL."
  [mapping-name & body]
  (let [opts (if (map? (first body)) (first body) {})
        fields (if (map? (first body)) (rest body) body)]
    `(register-mapping! ~(keyword mapping-name)
                        ~(vec fields)
                        ~@(mapcat identity opts))))

(defn field
  "Create a field mapping."
  [source & {:keys [target transformer converter default required]}]
  {:source source
   :target (or target source)
   :transformer transformer
   :converter converter
   :default default
   :required required})

(defn rename
  "Create a rename field mapping."
  [source target]
  {:source source :target target})

(defn transform
  "Create a transform field mapping."
  [source transformer & {:keys [target]}]
  {:source source :target (or target source) :transformer transformer})

(defn with-default
  "Create a field mapping with a default value."
  [source default-value & {:keys [target]}]
  {:source source :target (or target source) :default default-value})

;; =============================================================================
;; BUILT-IN TRANSFORMERS
;; =============================================================================

(defn init-built-in-transformers!
  "Initialize built-in transformers."
  []
  ;; String transformers
  (register-transformer! :uppercase str/upper-case :description "Convert to uppercase")
  (register-transformer! :lowercase str/lower-case :description "Convert to lowercase")
  (register-transformer! :trim str/trim :description "Trim whitespace")
  (register-transformer! :capitalize str/capitalize :description "Capitalize first letter")
  ;; Number transformers
  (register-transformer! :to-int #(if (number? %) (int %) (Integer/parseInt (str %)))
                         :description "Convert to integer")
  (register-transformer! :to-float #(if (number? %) (float %) (Float/parseFloat (str %)))
                         :description "Convert to float")
  (register-transformer! :to-string str :description "Convert to string")
  ;; Boolean transformers
  (register-transformer! :to-bool #(cond
                                     (boolean? %) %
                                     (string? %) (case (str/lower-case %)
                                                   ("true" "yes" "1") true
                                                   false)
                                     (number? %) (not (zero? %))
                                     :else (boolean %))
                         :description "Convert to boolean")
  ;; Collection transformers
  (register-transformer! :first first :description "Get first element")
  (register-transformer! :last last :description "Get last element")
  (register-transformer! :count count :description "Get count")
  (register-transformer! :flatten flatten :description "Flatten collection")
  (register-transformer! :distinct distinct :description "Get distinct elements")
  ;; Date transformers
  (register-transformer! :to-timestamp #(if (number? %) % (.getTime (java.util.Date.)))
                         :description "Convert to timestamp")
  ;; JSON transformers
  (register-transformer! :to-json #(clojure.data.json/write-str %)
                         :description "Convert to JSON string")
  (register-transformer! :from-json #(clojure.data.json/read-str % :key-fn keyword)
                         :description "Parse JSON string"))

;; =============================================================================
;; MAPPING VALIDATION
;; =============================================================================

(defn validate-mapping
  "Validate a mapping definition."
  [mapping-id]
  (if-let [mapping (get-mapping mapping-id)]
    (let [errors (atom [])]
      ;; Check for duplicate targets
      (let [targets (map :target (vals (:fields mapping)))
            duplicates (filter #(> (count %) 1) (vals (group-by identity targets)))]
        (when (seq duplicates)
          (swap! errors conj {:error :duplicate-targets
                              :targets (map first duplicates)})))
      ;; Check for missing transformers
      (doseq [[_ field-mapping] (:fields mapping)]
        (when-let [transformer (:transformer field-mapping)]
          (when-not (get-transformer transformer)
            (swap! errors conj {:error :missing-transformer
                                :transformer transformer
                                :field (:source field-mapping)}))))
      ;; Check for missing nested mappings
      (doseq [[field nested-mapping-id] (:nested mapping)]
        (when-not (get-mapping nested-mapping-id)
          (swap! errors conj {:error :missing-nested-mapping
                              :mapping nested-mapping-id
                              :field field})))
      {:valid (empty? @errors)
       :errors @errors})
    {:valid false
     :errors [{:error :mapping-not-found :mapping-id mapping-id}]}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-mapper-stats
  "Get data mapper statistics."
  []
  {:total-mappings (.size ^ConcurrentHashMap (:mappings @mapper-state))
   :total-transformers (.size ^ConcurrentHashMap (:transformers @mapper-state))
   :total-converters (count (:converters @mapper-state))
   :objects-mapped (.get ^AtomicLong (:mapping-count @mapper-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-data-mapper!
  "Initialize data mapper."
  []
  (log/info "Initializing data mapper")
  ;; Register feature flag
  (flags/register-flag! "data-mapper" "Enable data mapper" true)
  ;; Create metrics
  (metrics/create-counter! :datamapper/mappings-registered "Mappings registered")
  (metrics/create-counter! :datamapper/objects-mapped "Objects mapped")
  (metrics/create-gauge! :datamapper/total-mappings "Total mappings"
                         #(.size ^ConcurrentHashMap (:mappings @mapper-state)))
  ;; Initialize built-in transformers
  (init-built-in-transformers!)
  (log/info "Data mapper initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-data-mapper-status []
  {:enabled (flags/is-enabled? "data-mapper")
   :mappings (.size ^ConcurrentHashMap (:mappings @mapper-state))
   :transformers (.size ^ConcurrentHashMap (:transformers @mapper-state))
   :converters (count (:converters @mapper-state))
   :objects-mapped (.get ^AtomicLong (:mapping-count @mapper-state))
   :stats (get-mapper-stats)
   :config (:config @mapper-state)})
