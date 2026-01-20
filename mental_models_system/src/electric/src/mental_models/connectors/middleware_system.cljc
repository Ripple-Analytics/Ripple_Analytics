(ns mental-models.connectors.middleware-system
  "Connectors Module - Middleware System"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================
;; Middleware System
;; ============================================

(def middleware-registry
  "Atom storing middleware functions.
   Middleware can transform requests before sending and responses after receiving."
  (atom {:request []
         :response []}))

#?(:clj
   (defn register-middleware
     "Register a middleware function.
      Type: :request (transforms request before sending) or :response (transforms response after receiving)
      Middleware functions receive and return a map with request/response data.
      Returns middleware-id for unregistering."
     [middleware-type middleware-fn & {:keys [name priority] :or {priority 50}}]
     (let [middleware-id (java.util.UUID/randomUUID)]
       (swap! middleware-registry update middleware-type
              (fn [middlewares]
                (->> (conj middlewares {:id middleware-id
                                        :fn middleware-fn
                                        :name name
                                        :priority priority})
                     (sort-by :priority))))
       middleware-id)))

#?(:clj
   (defn unregister-middleware
     "Unregister a middleware by its ID."
     [middleware-type middleware-id]
     (swap! middleware-registry update middleware-type
            (fn [middlewares]
              (vec (remove #(= middleware-id (:id %)) middlewares))))))

#?(:clj
   (defn clear-middleware
     "Clear all middleware or middleware of a specific type."
     ([] (reset! middleware-registry {:request [] :response []}))
     ([middleware-type] (swap! middleware-registry assoc middleware-type []))))

#?(:clj
   (defn list-middleware
     "List all registered middleware."
     ([] @middleware-registry)
     ([middleware-type] (get @middleware-registry middleware-type []))))

#?(:clj
   (defn apply-request-middleware
     "Apply all request middleware to transform a request."
     [request]
     (reduce (fn [req {:keys [fn]}]
               (try
                 (fn req)
                 (catch Exception e
                   (println "Request middleware error:" (.getMessage e))
                   req)))
             request
             (get @middleware-registry :request []))))

#?(:clj
   (defn apply-response-middleware
     "Apply all response middleware to transform a response."
     [response]
     (reduce (fn [resp {:keys [fn]}]
               (try
                 (fn resp)
                 (catch Exception e
                   (println "Response middleware error:" (.getMessage e))
                   resp)))
             response
             (get @middleware-registry :response []))))

;; Built-in middleware examples

#?(:clj
   (defn create-logging-middleware
     "Create a middleware that logs requests/responses."
     [log-fn]
     {:request (fn [req]
                 (log-fn :request req)
                 req)
      :response (fn [resp]
                  (log-fn :response resp)
                  resp)}))

#?(:clj
   (defn create-retry-header-middleware
     "Create a middleware that adds retry headers to requests."
     []
     (fn [req]
       (update req :headers assoc "X-Retry-Count" (str (get req :retry-count 0))))))

#?(:clj
   (defn create-timing-middleware
     "Create a middleware that adds timing information to responses."
     []
     (fn [resp]
       (assoc resp :middleware-processed-at (java.time.Instant/now)))))

#?(:clj
   (defn create-error-transform-middleware
     "Create a middleware that transforms error responses to a standard format."
     []
     (fn [resp]
       (if (and (map? resp) (or (:error resp) (>= (get resp :status 0) 400)))
         (assoc resp :standardized-error
                {:message (or (:error resp) "Request failed")
                 :status (get resp :status "NA")
                 :timestamp (java.time.Instant/now)
                 :recoverable (contains? #{408 429 500 502 503 504} (:status resp))})
         resp))))

;; ============================================
;; Async Connector Support
