(ns mental-models.desktop.core
  "Desktop App - ClojureScript + Electron
   Replaces Rust/Tauri desktop app
   Provides folder watching, file ingestion, and real-time sync with web app"
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [cljs.core.async :as async :refer [<! >! chan go]]
            [cljs-http.client :as http]))

;; -- Electron IPC Bridge --

(def ipc (.-ipcRenderer (js/require "electron")))

(defn send-to-main [channel data]
  (.send ipc channel (clj->js data)))

(defn listen-from-main [channel callback]
  (.on ipc channel (fn [_event data]
                    (callback (js->clj data :keywordize-keys true)))))

;; -- Application State --

(defonce app-state (r/atom
  {:user nil
   :watched-folders []
   :recent-files []
   :analysis-results []
   :sync-status :idle
   :api-url (or (.-API_URL js/process.env) "http://localhost:3000")}))

;; -- File System Watching --

(def fs (js/require "fs"))
(def path-module (js/require "path"))
(def chokidar (js/require "chokidar"))

(declare read-file analyze-file)

(defn watch-folder
  "Watch folder for new files"
  [folder-path]
  (let [watcher (.watch chokidar folder-path
                       #js {:ignored #"(^\.)|(\\.swp$)"
                            :persistent true
                            :awaitWriteFinish #js {:stabilityThreshold 2000
                                                   :pollInterval 100}})]
    
    ;; Listen for add events
    (.on watcher "add" (fn [filepath]
                         (let [ext (.extname path-module filepath)]
                           (when (contains? #{".txt" ".md" ".pdf" ".docx" ".csv" ".json"} ext)
                             (go
                               (let [content (<! (read-file filepath))
                                     result (<! (analyze-file filepath content))]
                                 (swap! app-state update :analysis-results conj result)
                                 (send-to-main "file-analyzed" result)))))))
    
    ;; Store watcher reference
    (swap! app-state update :watched-folders conj
           {:path folder-path :watcher watcher})))

(defn unwatch-folder
  "Stop watching a folder"
  [folder-path]
  (let [watcher (->> (:watched-folders @app-state)
                    (filter #(= (:path %) folder-path))
                    first
                    :watcher)]
    (when watcher
      (.close watcher))
    (swap! app-state update :watched-folders
           (fn [folders] (filter #(not= (:path %) folder-path) folders)))))

;; -- File Reading --

(defn read-file
  "Read file asynchronously"
  [filepath]
  (let [ch (chan)]
    (.readFile fs filepath "utf8" (fn [err data]
                                    (if err
                                      (do
                                        (js/console.error "Error reading file:" err)
                                        (async/close! ch))
                                      (go (>! ch data)))))
    ch))

(defn read-pdf
  "Read PDF file"
  [filepath]
  (let [ch (chan)
        pdf-parse (js/require "pdf-parse")]
    (.readFile fs filepath (fn [err data]
                             (if err
                               (do
                                 (js/console.error "Error reading PDF:" err)
                                 (async/close! ch))
                               (pdf-parse data (fn [parse-err result]
                                                 (if parse-err
                                                   (do
                                                     (js/console.error "Error parsing PDF:" parse-err)
                                                     (async/close! ch))
                                                   (go (>! ch (.-text result)))))))))
    ch))

;; -- API Communication --

(defn analyze-file
  "Send file for analysis to backend"
  [filepath content]
  (let [ch (chan)
        api-url (:api-url @app-state)]
    
    (go
      (try
        (let [response (<! (http/post (str api-url "/api/v1/analyze")
                                     {:json-params {:text content
                                                   :source "file"
                                                   :source-url filepath}}))]
          (if (:success response)
            (>! ch (:body response))
            (do
              (js/console.error "Analysis failed:" (:error-text response))
              (>! ch nil))))
        (catch js/Error e
          (js/console.error "API error:" e)
          (>! ch nil))))
    
    ch))

(defn sync-results
  "Sync analysis results to backend"
  [results]
  (let [ch (chan)
        api-url (:api-url @app-state)]
    
    (swap! app-state assoc :sync-status :syncing)
    
    (go
      (try
        (let [response (<! (http/post (str api-url "/api/v1/analyze/batch")
                                     {:json-params {:analyses results}}))]
          (if (:success response)
            (do
              (swap! app-state assoc :sync-status :synced)
              (>! ch true))
            (do
              (swap! app-state assoc :sync-status :error)
              (>! ch false))))
        (catch js/Error e
          (swap! app-state assoc :sync-status :error)
          (>! ch false))))
    
    ch))

;; -- React Components --

(defn folder-list
  "Display watched folders"
  []
  [:div.folder-list
   [:h3 "Watched Folders"]
   (if (empty? (:watched-folders @app-state))
     [:p "No folders being watched"]
     [:ul
      (for [folder (:watched-folders @app-state)]
        ^{:key (:path folder)}
        [:li
         [:span (:path folder)]
         [:button {:on-click #(unwatch-folder (:path folder))}
          "Stop Watching"]])])])

(defn recent-analyses
  "Display recent analysis results"
  []
  [:div.recent-analyses
   [:h3 "Recent Analyses"]
   (if (empty? (:analysis-results @app-state))
     [:p "No analyses yet"]
     [:ul
      (for [result (take 10 (reverse (:analysis-results @app-state)))]
        ^{:key (:id result)}
        [:li
         [:div.result-item
          [:span.filename (:source-url result)]
          [:span.score (str "Score: " (.toFixed (:average-score result) 2))]])])])

(defn sync-status-indicator
  "Display sync status"
  []
  (let [status (:sync-status @app-state)]
    [:div.sync-status
     [:span {:class (str "status-" (name status))}
      (case status
        :idle "Ready"
        :syncing "Syncing..."
        :synced "Synced"
        :error "Sync Error")]]))

(defn add-folder-dialog
  "Dialog to add new folder"
  []
  (let [folder-input (r/atom "")]
    (fn []
      [:div.add-folder-dialog
       [:input {:type "text"
               :placeholder "Folder path..."
               :value @folder-input
               :on-change #(reset! folder-input (-> % .-target .-value))}]
       [:button {:on-click (fn []
                           (when (not (str/blank? @folder-input))
                             (watch-folder @folder-input)
                             (reset! folder-input "")))}
        "Watch Folder"]])))

(defn app
  "Main desktop app component"
  []
  [:div.desktop-app
   [:header
    [:h1 "Mental Models Desktop"]
    [sync-status-indicator]]
   
   [:main
    [:section
     [add-folder-dialog]
     [folder-list]
     [recent-analyses]]
    
    [:section.actions
     [:button {:on-click (fn []
                         (go
                           (let [result (<! (sync-results (:analysis-results @app-state)))]
                             (if result
                               (js/alert "Synced successfully!")
                               (js/alert "Sync failed")))))}
      "Sync Results"]
     [:button {:on-click #(send-to-main "open-web-app" {})}
      "Open Web App"]]]])

;; -- Electron IPC Handlers --

(listen-from-main "add-folder" (fn [data]
                                  (watch-folder (:path data))))

(listen-from-main "remove-folder" (fn [data]
                                     (unwatch-folder (:path data))))

(listen-from-main "sync-all" (fn [_data]
                               (go
                                 (<! (sync-results (:analysis-results @app-state))))))

;; -- Initialization --

(defn init []
  (rdom/render [app]
              (js/document.getElementById "app")))

(init)

;; Hot reload support
(defn ^:dev/after-load reload []
  (init))
