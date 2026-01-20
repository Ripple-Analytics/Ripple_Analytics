(ns mental-models.connectors.section-29
  "Connectors Module - Section 29"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def di-container
  "Dependency injection container for connectors.
   Allows swapping implementations for testing or different environments."
  (atom {:http-client nil
         :cache nil
         :metrics nil
         :logger nil
         :environment :production}))

#?(:clj
   (defn register-dependency
     "Register a dependency in the DI container."
     [key implementation]
     (swap! di-container assoc key implementation)))

#?(:clj
   (defn get-dependency
     "Get a dependency from the DI container."
     [key]
     (get @di-container key)))

#?(:clj
   (defn set-environment
     "Set the current environment (e.g., :production, :development, :test)."
     [env]
     (swap! di-container assoc :environment env)))

#?(:clj
   (defn get-environment
     "Get the current environment."
     []
     (get @di-container :environment)))

#?(:clj
   (defn with-test-dependencies
     "Execute a function with test dependencies injected.
      Restores original dependencies after execution."
     [test-deps f]
     (let [original @di-container]
       (try
         (swap! di-container merge test-deps {:environment :test})
         (f)
         (finally
           (reset! di-container original))))))

#?(:clj
   (defn create-mock-http-client
     "Create a mock HTTP client for testing.
      Responses is a map of URL patterns to response data."
     [responses]
     {:type :mock
      :responses responses
      :request-log (atom [])
      :make-request (fn [method url opts]
                      (let [response (or (get responses url)
                                         (some (fn [[pattern resp]]
                                                 (when (and (instance? java.util.regex.Pattern pattern)
                                                            (re-matches pattern url))
                                                   resp))
                                               responses)
                                         {:status 404 :body "Not found"})]
                        (swap! (:request-log (get-dependency :http-client))
                               conj {:method method :url url :opts opts})
                        response))}))

#?(:clj
   (defn create-mock-cache
     "Create a mock cache for testing."
     []
     (let [store (atom {})]
       {:type :mock
        :store store
        :get (fn [key] (get @store key))
        :put (fn [key value ttl] (swap! store assoc key {:value value :ttl ttl}))
        :invalidate (fn [key] (swap! store dissoc key))
        :clear (fn [] (reset! store {}))})))

#?(:clj
   (defn create-mock-logger
     "Create a mock logger for testing."
     []
     (let [logs (atom [])]
       {:type :mock
        :logs logs
        :log (fn [level message & args]
               (swap! logs conj {:level level :message message :args args :timestamp (java.time.Instant/now)}))
        :get-logs (fn [] @logs)
        :clear (fn [] (reset! logs []))})))

#?(:clj
   (defn inject-connector
     "Create a connector with injected dependencies.
      Allows overriding default implementations for testing."
     [connector-type config & {:keys [http-client cache logger]}]
     (let [base-connector (create-connector connector-type config)]
       (if (:error base-connector)
         base-connector
         (assoc base-connector
                :injected-http-client (or http-client (get-dependency :http-client))
                :injected-cache (or cache (get-dependency :cache))
                :injected-logger (or logger (get-dependency :logger))
                :di-enabled true)))))

#?(:clj
   (defn get-injected-http-client
     "Get the HTTP client for a connector, using injected one if available."
     [connector]
     (or (:injected-http-client connector)
         (get-dependency :http-client))))

#?(:clj
   (defn get-injected-cache
     "Get the cache for a connector, using injected one if available."
     [connector]
     (or (:injected-cache connector)
         (get-dependency :cache))))

#?(:clj
   (defn get-injected-logger
     "Get the logger for a connector, using injected one if available."
     [connector]
     (or (:injected-logger connector)
         (get-dependency :logger))))

;; ============================================
;; Connector Pool Manager
