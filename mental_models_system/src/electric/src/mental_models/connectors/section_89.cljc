(ns mental-models.connectors.section-89
  "Connectors Module - Section 89"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def connector-aliases
  "Registry of connector aliases."
  (atom {}))

(def alias-config
  "Configuration for connector aliasing."
  (atom {:enabled true
         :allow-chaining true
         :max-chain-depth 5}))

(defn get-alias-config
  "Get current alias configuration."
  []
  @alias-config)

(defn set-alias-config
  "Update alias configuration."
  [config]
  (swap! alias-config merge config))

(defn register-alias
  "Register an alias for a connector."
  [alias-name target-connector & {:keys [config-overrides description]}]
  (swap! connector-aliases assoc alias-name
         {:target target-connector
          :config-overrides (or config-overrides {})
          :description description
          :created-at (System/currentTimeMillis)}))

(defn unregister-alias
  "Remove a connector alias."
  [alias-name]
  (swap! connector-aliases dissoc alias-name))

(defn resolve-alias
  "Resolve an alias to its target connector, following chains if allowed."
  [alias-name & {:keys [depth] :or {depth 0}}]
  (let [config @alias-config]
    (if (> depth (:max-chain-depth config))
      {:error "Max alias chain depth exceeded"}
      (if-let [alias-info (get @connector-aliases alias-name)]
        (let [target (:target alias-info)]
          (if (and (:allow-chaining config) (contains? @connector-aliases target))
            (resolve-alias target :depth (inc depth))
            {:connector target
             :config-overrides (:config-overrides alias-info)}))
        {:connector alias-name
         :config-overrides {}}))))

(defn list-aliases
  "List all registered aliases."
  []
  (mapv (fn [[k v]]
          {:alias k
           :target (:target v)
           :description (:description v)})
        @connector-aliases))

(defn get-alias-stats
  "Get statistics about connector aliases."
  []
  {:total-aliases (count @connector-aliases)
   :config @alias-config
   :aliases (keys @connector-aliases)})

;; ============================================
;; Request Replay
