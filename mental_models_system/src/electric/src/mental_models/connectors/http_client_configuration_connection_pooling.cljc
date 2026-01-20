(ns mental-models.connectors.http-client-configuration-connection-pooling
  "Connectors Module - Http Client Configuration Connection Pooling"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================
;; HTTP Client Configuration & Connection Pooling
;; ============================================

#?(:clj
   (def connection-manager
     "Shared connection manager for HTTP connection pooling.
      Reuses connections across requests for better performance."
     (delay
       (clj-http.conn-mgr/make-reusable-conn-manager
        {:timeout 30
         :threads 8
         :default-per-route 4
         :insecure? false}))))

#?(:clj
   (def default-http-opts
     "Default HTTP options with connection pooling."
     {:connection-manager @connection-manager
      :socket-timeout 30000
      :connection-timeout 10000
      :throw-exceptions false}))

;; ============================================
;; Configurable Timeout Settings
;; ============================================

(def timeout-config
  "Configurable timeout settings per connector type (milliseconds)."
  (atom {:github {:socket-timeout 30000 :connection-timeout 10000}
         :slack {:socket-timeout 30000 :connection-timeout 10000}
         :huggingface {:socket-timeout 60000 :connection-timeout 15000}
         :lm-studio {:socket-timeout 120000 :connection-timeout 10000}
         :web-scraper {:socket-timeout 45000 :connection-timeout 15000}
         :zapier {:socket-timeout 30000 :connection-timeout 10000}
         :file {:socket-timeout 5000 :connection-timeout 1000}
         :default {:socket-timeout 30000 :connection-timeout 10000}}))

#?(:clj
   (defn get-timeout-config
     "Get timeout configuration for a connector type."
     [connector-type]
     (get @timeout-config connector-type (get @timeout-config :default))))

#?(:clj
   (defn set-timeout-config
     "Set timeout configuration for a connector type."
     [connector-type socket-timeout connection-timeout]
     (swap! timeout-config assoc connector-type
            {:socket-timeout socket-timeout
             :connection-timeout connection-timeout})))

#?(:clj
   (defn get-http-opts-for-connector
     "Get HTTP options with connector-specific timeouts."
     [connector-type]
     (let [timeouts (get-timeout-config connector-type)]
       (merge default-http-opts timeouts))))

;; ============================================
;; Event Hooks System
;; ============================================

(def event-hooks
  "Atom storing event hook functions.
   Events: :request-start, :request-complete, :request-error, 
           :circuit-open, :circuit-close, :rate-limit-hit, :cache-hit, :cache-miss"
  (atom {:request-start []
         :request-complete []
         :request-error []
         :circuit-open []
         :circuit-close []
         :rate-limit-hit []
         :cache-hit []
         :cache-miss []}))

#?(:clj
   (defn register-hook
     "Register a hook function for an event type.
      Hook functions receive event data as a map.
      Returns a hook-id that can be used to unregister."
     [event-type hook-fn]
     (let [hook-id (java.util.UUID/randomUUID)]
       (swap! event-hooks update event-type conj {:id hook-id :fn hook-fn})
       hook-id)))

#?(:clj
   (defn unregister-hook
     "Unregister a hook by its ID."
     [event-type hook-id]
     (swap! event-hooks update event-type
            (fn [hooks]
              (vec (remove #(= hook-id (:id %)) hooks))))))

#?(:clj
   (defn clear-hooks
     "Clear all hooks for an event type, or all hooks if no type specified."
     ([] (reset! event-hooks {:request-start []
                              :request-complete []
                              :request-error []
                              :circuit-open []
                              :circuit-close []
                              :rate-limit-hit []
                              :cache-hit []
                              :cache-miss []}))
     ([event-type] (swap! event-hooks assoc event-type []))))

#?(:clj
   (defn emit-event
     "Emit an event to all registered hooks.
      Event data should include :event-type and relevant context."
     [event-type event-data]
     (let [hooks (get @event-hooks event-type [])
           full-event (assoc event-data
                             :event-type event-type
                             :timestamp (java.time.Instant/now))]
       (doseq [{:keys [fn]} hooks]
         (try
           (fn full-event)
           (catch Exception e
             ;; Log but don't fail on hook errors
             (println "Hook error for" event-type ":" (.getMessage e))))))))

#?(:clj
   (defn list-hooks
     "List all registered hooks, optionally filtered by event type."
     ([] @event-hooks)
     ([event-type] (get @event-hooks event-type []))))

;; ============================================
;; Retry Logic with Exponential Backoff
