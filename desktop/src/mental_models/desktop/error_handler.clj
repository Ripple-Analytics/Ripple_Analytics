(ns mental-models.desktop.error-handler
  "Global error handling with Slack reporting and hot-reload support.
   Catches all exceptions, reports to Slack for Devin auto-fix, and
   hot-reloads fixes from GitHub without app restart."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.stacktrace :as stacktrace])
  (:import [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [java.net HttpURLConnection URL]
           [java.io OutputStreamWriter]))

;; =============================================================================
;; State
;; =============================================================================

(def ^:private error-state
  (atom {:errors []
         :last-error nil
         :error-count 0
         :slack-webhook nil
         :slack-channel nil
         :github-repo "Ripple-Analytics/Ripple_Analytics"
         :auto-report true
         :listeners []}))

;; =============================================================================
;; Error Formatting
;; =============================================================================

(defn- format-timestamp []
  (.format (LocalDateTime/now)
           (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")))

(defn- extract-source-info [^Throwable ex]
  "Extract file, line, and function from exception stack trace"
  (let [trace (.getStackTrace ex)
        relevant (->> trace
                      (filter #(str/includes? (.getClassName %) "mental_models"))
                      first)]
    (when relevant
      {:file (.getFileName relevant)
       :line (.getLineNumber relevant)
       :class (.getClassName relevant)
       :method (.getMethodName relevant)})))

(defn- format-stack-trace [^Throwable ex]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)]
    (.printStackTrace ex pw)
    (str sw)))

(defn- format-error-for-slack [^Throwable ex context]
  "Format error for Slack with Devin-friendly structure"
  (let [source (extract-source-info ex)
        stack (format-stack-trace ex)]
    {:type "error_report"
     :timestamp (format-timestamp)
     :app "Mental Models Desktop"
     :version (or (System/getProperty "app.version") "1.0.6")
     :error {:message (.getMessage ex)
             :type (.getName (class ex))
             :source source
             :context context}
     :stack_trace (take 20 (str/split-lines stack))
     :devin_task {:action "fix_error"
                  :repo (:github-repo @error-state)
                  :file (:file source)
                  :line (:line source)
                  :description (str "Fix " (.getName (class ex)) 
                                    " in " (:file source) 
                                    " at line " (:line source)
                                    ": " (.getMessage ex))}}))

(defn- format-slack-message [error-data]
  "Format as Slack Block Kit message"
  (let [{:keys [error stack_trace devin_task]} error-data]
    {:blocks
     [{:type "header"
       :text {:type "plain_text"
              :text "🚨 Mental Models Desktop Error"}}
      {:type "section"
       :fields [{:type "mrkdwn"
                 :text (str "*Error Type:*\n" (:type error))}
                {:type "mrkdwn"
                 :text (str "*File:*\n" (get-in error [:source :file] "unknown"))}
                {:type "mrkdwn"
                 :text (str "*Line:*\n" (get-in error [:source :line] "?"))}
                {:type "mrkdwn"
                 :text (str "*Time:*\n" (:timestamp error-data))}]}
      {:type "section"
       :text {:type "mrkdwn"
              :text (str "*Message:*\n```" (:message error) "```")}}
      {:type "section"
       :text {:type "mrkdwn"
              :text (str "*Stack Trace (first 10 lines):*\n```"
                         (str/join "\n" (take 10 stack_trace))
                         "```")}}
      {:type "divider"}
      {:type "section"
       :text {:type "mrkdwn"
              :text (str "*@Devin Task:*\n" (:description devin_task))}}
      {:type "context"
       :elements [{:type "mrkdwn"
                   :text (str "Repo: `" (:repo devin_task) "` | Auto-fix enabled")}]}]}))

;; =============================================================================
;; Slack Integration
;; =============================================================================

(defn- send-to-slack! [error-data]
  "Send error to Slack webhook"
  (when-let [webhook (:slack-webhook @error-state)]
    (try
      (let [url (URL. webhook)
            conn ^HttpURLConnection (.openConnection url)
            message (format-slack-message error-data)
            json (str "{\"blocks\":" 
                      (pr-str (:blocks message))
                      "}")]
        (.setRequestMethod conn "POST")
        (.setRequestProperty conn "Content-Type" "application/json")
        (.setDoOutput conn true)
        
        (with-open [writer (OutputStreamWriter. (.getOutputStream conn))]
          (.write writer json))
        
        (let [response-code (.getResponseCode conn)]
          (when (not= response-code 200)
            (println "[ERROR] Slack webhook failed:" response-code))))
      (catch Exception e
        (println "[ERROR] Failed to send to Slack:" (.getMessage e))))))

;; =============================================================================
;; Error Storage
;; =============================================================================

(defn- store-error! [error-data]
  "Store error in local state for UI display"
  (swap! error-state
         (fn [state]
           (-> state
               (update :errors #(take 100 (conj % error-data)))
               (assoc :last-error error-data)
               (update :error-count inc)))))

(defn- notify-listeners! [error-data]
  "Notify UI listeners of new error"
  (doseq [listener (:listeners @error-state)]
    (try
      (listener error-data)
      (catch Exception _))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn configure!
  "Configure error handler with Slack webhook and options"
  [{:keys [slack-webhook slack-channel github-repo auto-report]}]
  (swap! error-state merge
         (cond-> {}
           slack-webhook (assoc :slack-webhook slack-webhook)
           slack-channel (assoc :slack-channel slack-channel)
           github-repo (assoc :github-repo github-repo)
           (some? auto-report) (assoc :auto-report auto-report))))

(defn add-listener!
  "Add a listener function that gets called on each error"
  [listener-fn]
  (swap! error-state update :listeners conj listener-fn))

(defn remove-listener!
  "Remove a listener function"
  [listener-fn]
  (swap! error-state update :listeners #(remove #{listener-fn} %)))

(defn handle-error!
  "Handle an exception - log, store, report to Slack, notify UI"
  ([^Throwable ex]
   (handle-error! ex {}))
  ([^Throwable ex context]
   (let [error-data (format-error-for-slack ex context)]
     ;; Log locally
     (println "\n[ERROR]" (format-timestamp) "-" (.getMessage ex))
     (println "  File:" (get-in error-data [:error :source :file]))
     (println "  Line:" (get-in error-data [:error :source :line]))
     
     ;; Store for UI
     (store-error! error-data)
     
     ;; Send to Slack if configured
     (when (:auto-report @error-state)
       (future (send-to-slack! error-data)))
     
     ;; Notify UI listeners
     (notify-listeners! error-data)
     
     ;; Return error data for caller
     error-data)))

(defn get-errors
  "Get list of recent errors"
  []
  (:errors @error-state))

(defn get-last-error
  "Get the most recent error"
  []
  (:last-error @error-state))

(defn get-error-count
  "Get total error count"
  []
  (:error-count @error-state))

(defn clear-errors!
  "Clear error history"
  []
  (swap! error-state assoc :errors [] :last-error nil))

;; =============================================================================
;; Global Exception Handler
;; =============================================================================

(defn install-global-handler!
  "Install global uncaught exception handler"
  []
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (handle-error! ex {:thread (.getName thread)
                          :type :uncaught})))))

;; =============================================================================
;; Safe Execution Wrapper
;; =============================================================================

(defmacro safe
  "Execute body, catching any exceptions and handling them gracefully.
   Returns nil on error instead of crashing."
  [& body]
  `(try
     ~@body
     (catch Throwable e#
       (handle-error! e# {:context "safe-block"})
       nil)))

(defmacro safe-with-default
  "Execute body, returning default value on error"
  [default & body]
  `(try
     ~@body
     (catch Throwable e#
       (handle-error! e# {:context "safe-with-default"})
       ~default)))

(defn safe-fn
  "Wrap a function to catch exceptions and handle gracefully"
  [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Throwable e
        (handle-error! e {:context "safe-fn"
                          :function (str f)
                          :args (pr-str args)})
        nil))))

;; =============================================================================
;; Hot Reload Integration
;; =============================================================================

(def ^:private reload-state
  (atom {:checking false
         :last-check nil
         :pending-fixes []}))

(defn check-for-fixes!
  "Check GitHub for fixes to recent errors"
  []
  (when-not (:checking @reload-state)
    (swap! reload-state assoc :checking true)
    (future
      (try
        ;; This would integrate with the github_checker module
        ;; to pull latest changes and hot-reload
        (println "[INFO] Checking for fixes...")
        (swap! reload-state assoc :last-check (format-timestamp))
        (catch Exception e
          (println "[WARN] Fix check failed:" (.getMessage e)))
        (finally
          (swap! reload-state assoc :checking false))))))

(defn start-fix-watcher!
  "Start background thread that checks for fixes every 30 seconds after an error"
  []
  (future
    (loop []
      (Thread/sleep 30000)
      (when (and (pos? (:error-count @error-state))
                 (:auto-report @error-state))
        (check-for-fixes!))
      (recur))))
