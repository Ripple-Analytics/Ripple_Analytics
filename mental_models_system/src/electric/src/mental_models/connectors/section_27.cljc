(ns mental-models.connectors.section-27
  "Connectors Module - Section 27"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def config-schemas
  "Schema definitions for connector configurations.
   Each schema defines field types, constraints, and defaults."
  (atom {:github {:token {:type :string :required true :min-length 1 :sensitive true}
                  :base-url {:type :string :required false :default "https://api.github.com"}}
         :slack {:token {:type :string :required true :min-length 1 :sensitive true}
                 :base-url {:type :string :required false :default "https://slack.com/api"}}
         :huggingface {:api-key {:type :string :required true :min-length 1 :sensitive true}
                       :base-url {:type :string :required false :default "https://api-inference.huggingface.co"}}
         :lm-studio {:base-url {:type :string :required false :default "http://localhost:1234"}
                     :timeout-ms {:type :integer :required false :default 120000 :min 1000 :max 600000}}
         :zapier {:webhook-url {:type :string :required true :pattern #"^https://hooks\.zapier\.com/.*"}}
         :web-scraper {:user-agent {:type :string :required false :default "Mozilla/5.0"}
                       :max-retries {:type :integer :required false :default 3 :min 0 :max 10}}
         :file {:base-path {:type :string :required true :min-length 1}}}))

#?(:clj
   (defn validate-field
     "Validate a single field against its schema."
     [field-name value schema]
     (let [{:keys [type required min-length max-length min max pattern]} schema]
       (cond
         ;; Check required
         (and required (nil? value))
         {:valid false :error (str field-name " is required")}
         
         ;; Skip validation if nil and not required
         (nil? value)
         {:valid true}
         
         ;; Type validation
         (and (= type :string) (not (string? value)))
         {:valid false :error (str field-name " must be a string")}
         
         (and (= type :integer) (not (integer? value)))
         {:valid false :error (str field-name " must be an integer")}
         
         (and (= type :boolean) (not (boolean? value)))
         {:valid false :error (str field-name " must be a boolean")}
         
         ;; String constraints
         (and (= type :string) min-length (< (count value) min-length))
         {:valid false :error (str field-name " must be at least " min-length " characters")}
         
         (and (= type :string) max-length (> (count value) max-length))
         {:valid false :error (str field-name " must be at most " max-length " characters")}
         
         ;; Pattern validation
         (and (= type :string) pattern (not (re-matches pattern value)))
         {:valid false :error (str field-name " does not match required pattern")}
         
         ;; Numeric constraints
         (and (= type :integer) min (< value min))
         {:valid false :error (str field-name " must be at least " min)}
         
         (and (= type :integer) max (> value max))
         {:valid false :error (str field-name " must be at most " max)}
         
         :else
         {:valid true}))))

#?(:clj
   (defn validate-config
     "Validate a connector configuration against its schema.
      Returns {:valid true} or {:valid false :errors [...]}"
     [connector-type config]
     (if-let [schema (get @config-schemas connector-type)]
       (let [results (map (fn [[field-name field-schema]]
                            (validate-field (name field-name)
                                            (get config field-name)
                                            field-schema))
                          schema)
             errors (filter #(not (:valid %)) results)]
         (if (empty? errors)
           {:valid true :connector-type connector-type}
           {:valid false
            :connector-type connector-type
            :errors (mapv :error errors)}))
       {:valid true :connector-type connector-type :warning "No schema defined"})))

#?(:clj
   (defn apply-defaults
     "Apply default values from schema to config."
     [connector-type config]
     (if-let [schema (get @config-schemas connector-type)]
       (reduce (fn [cfg [field-name field-schema]]
                 (if (and (contains? field-schema :default)
                          (not (contains? cfg field-name)))
                   (assoc cfg field-name (:default field-schema))
                   cfg))
               config
               schema)
       config)))

#?(:clj
   (defn get-config-schema
     "Get the schema for a connector type."
     [connector-type]
     (get @config-schemas connector-type)))

#?(:clj
   (defn register-config-schema
     "Register a configuration schema for a connector type."
     [connector-type schema]
     (swap! config-schemas assoc connector-type schema)))

#?(:clj
   (defn list-config-schemas
     "List all registered configuration schemas."
     []
     (mapv (fn [[type schema]]
             {:type type
              :fields (mapv (fn [[field-name field-schema]]
                              {:name field-name
                               :type (:type field-schema)
                               :required (:required field-schema false)
                               :sensitive (:sensitive field-schema false)})
                            schema)})
           @config-schemas)))

#?(:clj
   (defn redact-sensitive-config
     "Redact sensitive fields from a config for logging/display."
     [connector-type config]
     (if-let [schema (get @config-schemas connector-type)]
       (reduce (fn [cfg [field-name field-schema]]
                 (if (and (:sensitive field-schema) (contains? cfg field-name))
                   (assoc cfg field-name "[REDACTED]")
                   cfg))
               config
               schema)
       config)))

;; ============================================
;; Dependency Injection Container
