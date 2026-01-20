(ns mental-models.connectors.section-25
  "Connectors Module - Section 25"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def connector-registry
  "Registry of connector types and their factory functions."
  (atom {}))

#?(:clj
   (defn register-connector-type
     "Register a connector type with its factory function.
      Factory function should accept a config map and return a connector."
     [connector-type factory-fn & {:keys [description required-config]}]
     (swap! connector-registry assoc connector-type
            {:factory-fn factory-fn
             :description description
             :required-config (or required-config [])
             :registered-at (java.time.Instant/now)})))

#?(:clj
   (defn unregister-connector-type
     "Unregister a connector type."
     [connector-type]
     (swap! connector-registry dissoc connector-type)))

#?(:clj
   (defn list-connector-types
     "List all registered connector types."
     []
     (mapv (fn [[type info]]
             {:type type
              :description (:description info)
              :required-config (:required-config info)})
           @connector-registry)))

#?(:clj
   (defn create-connector
     "Create a connector using the factory pattern.
      Validates required config and calls the registered factory function."
     [connector-type config]
     (if-let [type-info (get @connector-registry connector-type)]
       (let [required (:required-config type-info)
             missing (filter #(not (contains? config %)) required)]
         (if (seq missing)
           {:error (str "Missing required config: " (str/join ", " (map name missing)))
            :status :error}
           (try
             (let [connector ((:factory-fn type-info) config)]
               (emit-event :connector-created {:connector-type connector-type :config (dissoc config :api-key :token :password)})
               connector)
             (catch Exception e
               {:error (.getMessage e) :status :error}))))
       {:error (str "Unknown connector type: " (name connector-type))
        :status :error})))

#?(:clj
   (defn create-and-register
     "Create a connector and register it for lifecycle management."
     [name connector-type config]
     (let [connector (create-connector connector-type config)]
       (if (:error connector)
         connector
         (do
           (register-connector name connector-type connector)
           {:status :success
            :name name
            :connector-type connector-type
            :connector connector})))))

;; ============================================
;; Configuration Validation
