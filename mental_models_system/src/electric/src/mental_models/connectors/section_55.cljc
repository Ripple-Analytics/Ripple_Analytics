(ns mental-models.connectors.section-55
  "Connectors Module - Section 55"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def github-config
  {:base-url "https://api.github.com"
   :api-version "2022-11-28"})

(defn create-github-connector
  "Create a GitHub API connector."
  [token]
  {:type :github
   :token token
   :status :disconnected
   :rate-limit {:remaining 5000 :reset nil}})

(defn github-get-repo
  "Get repository information."
  [connector owner repo]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/repos/" owner "/" repo)
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:token connector))
                                           "Accept" "application/vnd.github+json"
                                           "X-GitHub-Api-Version" (:api-version github-config)}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:owner owner
          :repo repo
          :full-name (str owner "/" repo)
          :data (:body response)
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:owner owner
          :repo repo
          :full-name (str owner "/" repo)
          :data nil
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:owner owner
      :repo repo
      :status :error
      :error "GitHub API must be called from server"}))

(defn github-list-issues
  "List issues for a repository."
  [connector owner repo & {:keys [state] :or {state "open"}}]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/repos/" owner "/" repo "/issues")
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:token connector))
                                           "Accept" "application/vnd.github+json"
                                           "X-GitHub-Api-Version" (:api-version github-config)}
                                 :query-params {:state state}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:owner owner
          :repo repo
          :state state
          :issues (:body response)
          :total-count (count (:body response))
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:owner owner
          :repo repo
          :state state
          :issues []
          :total-count 0
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:owner owner
      :repo repo
      :issues []
      :status :error
      :error "GitHub API must be called from server"}))

(defn github-create-issue
  "Create a new issue."
  [connector owner repo title body]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/repos/" owner "/" repo "/issues")
             response (http/post url
                                 {:body (json/generate-string {:title title :body body})
                                  :headers {"Authorization" (str "Bearer " (:token connector))
                                            "Accept" "application/vnd.github+json"
                                            "X-GitHub-Api-Version" (:api-version github-config)}
                                  :content-type :json
                                  :socket-timeout 30000
                                  :connection-timeout 10000
                                  :as :json})]
         {:owner owner
          :repo repo
          :title title
          :body body
          :issue (:body response)
          :status :created
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:owner owner
          :repo repo
          :title title
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:owner owner
      :repo repo
      :status :error
      :error "GitHub API must be called from server"}))

(defn github-search-code
  "Search code in repositories."
  [connector query]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/search/code")
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:token connector))
                                           "Accept" "application/vnd.github+json"
                                           "X-GitHub-Api-Version" (:api-version github-config)}
                                 :query-params {:q query}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:query query
          :results (get-in response [:body :items])
          :total-count (get-in response [:body :total_count])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:query query
          :results []
          :total-count 0
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:query query
      :results []
      :status :error
      :error "GitHub API must be called from server"}))

