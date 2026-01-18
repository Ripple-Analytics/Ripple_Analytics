(ns mental-models.desktop.gui.app
  "Mental Models Desktop App - JavaFX GUI matching web app design"
  (:require [cljfx.api :as fx]
            [clojure.core.async :as async :refer [go go-loop <! >! chan]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [mental-models.desktop.error-handler :as errors])
  (:import [javafx.stage Stage FileChooser DirectoryChooser]
           [javafx.scene.control Alert Alert$AlertType]
           [javafx.application Platform]
           [java.awt SystemTray TrayIcon PopupMenu MenuItem]
           [java.awt.event ActionListener]
           [javax.imageio ImageIO]))

;; =============================================================================
;; State Management
;; =============================================================================

(def *state
  (atom {:current-view :dashboard
         :sidebar-collapsed false
         
         ;; Dashboard stats
         :stats {:files-scanned 0
                 :models-found 0
                 :documents-analyzed 0
                 :lollapalooza-events 0
                 :last-sync nil}
         
         ;; Scan state
         :scan {:status :idle  ;; :idle :scanning :watching :paused
                :current-file nil
                :progress 0
                :total-files 0
                :found-models []}
         
         ;; Watched folders
         :watched-folders []
         
         ;; Logs
         :logs []
         :log-filter :all  ;; :all :info :warning :error :model
         
         ;; Settings
         :settings {:lm-studio-url "http://localhost:1234"
                    :web-app-url ""
                    :slack-webhook ""
                    :slack-channel ""
                    :auto-update true
                    :update-interval 5
                    :theme :light}
         
         ;; Connection status
         :connections {:lm-studio :disconnected
                       :web-app :disconnected
                       :slack :disconnected}
         
         ;; Updates
         :updates {:available false
                   :current-version "1.0.0"
                   :latest-version nil
                   :last-check nil
                   :last-updated nil  ;; When app was last updated from GitHub
                   :checking false
                   :outdated-days 0}  ;; Days since last update))

;; =============================================================================
;; Color Scheme (Light Theme)
;; =============================================================================

(def colors
  {:bg-primary "#ffffff"
   :bg-secondary "#f8fafc"
   :bg-tertiary "#f1f5f9"
   :bg-card "#ffffff"
   :border "#e2e8f0"
   :text-primary "#0f172a"
   :text-secondary "#475569"
   :text-muted "#94a3b8"
   :accent "#3b82f6"
   :accent-hover "#2563eb"
   :success "#22c55e"
   :warning "#f59e0b"
   :error "#ef4444"
   :purple "#a855f7"})

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn add-log! [level message & [details]]
  (swap! *state update :logs conj
         {:id (java.util.UUID/randomUUID)
          :timestamp (java.time.Instant/now)
          :level level
          :message message
          :details details}))

(defn update-stat! [key delta]
  (swap! *state update-in [:stats key] + delta))

(defn set-connection! [service status]
  (swap! *state assoc-in [:connections service] status))

;; =============================================================================
;; Sidebar Component
;; =============================================================================

(defn sidebar-item [{:keys [icon label view active? on-click]}]
  {:fx/type :h-box
   :style-class (if active? "sidebar-item active" "sidebar-item")
   :alignment :center-left
   :spacing 12
   :padding {:left 16 :right 16 :top 12 :bottom 12}
   :on-mouse-clicked on-click
   :children [{:fx/type :label
               :text icon
               :style {:-fx-font-size 16}}
              {:fx/type :label
               :text label
               :style {:-fx-font-size 14
                       :-fx-text-fill (:text-primary colors)}}]})

(defn sidebar [{:keys [current-view on-navigate]}]
  {:fx/type :v-box
   :style {:-fx-background-color (:bg-secondary colors)
           :-fx-border-color (:border colors)
           :-fx-border-width "0 1 0 0"}
   :pref-width 220
   :children [;; Logo/Title
              {:fx/type :h-box
               :alignment :center-left
               :padding 20
               :spacing 12
               :children [{:fx/type :label
                           :text "🧠"
                           :style {:-fx-font-size 24}}
                          {:fx/type :label
                           :text "Mental Models"
                           :style {:-fx-font-size 16
                                   :-fx-font-weight :bold
                                   :-fx-text-fill (:text-primary colors)}}]}
              
              ;; Navigation
              {:fx/type :v-box
               :spacing 4
               :padding {:left 8 :right 8}
               :children [(sidebar-item {:icon "📊" :label "Dashboard" :view :dashboard
                                         :active? (= current-view :dashboard)
                                         :on-click {:event/type ::navigate :view :dashboard}})
                          (sidebar-item {:icon "📁" :label "Scan Folder" :view :scan
                                         :active? (= current-view :scan)
                                         :on-click {:event/type ::navigate :view :scan}})
                          (sidebar-item {:icon "👁" :label "Watch Mode" :view :watch
                                         :active? (= current-view :watch)
                                         :on-click {:event/type ::navigate :view :watch}})
                          (sidebar-item {:icon "📋" :label "Live Logs" :view :logs
                                         :active? (= current-view :logs)
                                         :on-click {:event/type ::navigate :view :logs}})
                          (sidebar-item {:icon "⚙️" :label "Settings" :view :settings
                                         :active? (= current-view :settings)
                                         :on-click {:event/type ::navigate :view :settings}})]}
              
              ;; Spacer
              {:fx/type :region
               :v-box/vgrow :always}
              
              ;; Connection Status
              {:fx/type :v-box
               :padding 16
               :spacing 8
               :children [{:fx/type :label
                           :text "CONNECTIONS"
                           :style {:-fx-font-size 10
                                   :-fx-text-fill (:text-muted colors)}}
                          {:fx/type :h-box
                           :spacing 8
                           :alignment :center-left
                           :children [{:fx/type :label
                                       :text "●"
                                       :style {:-fx-text-fill (case (:lm-studio (:connections @*state))
                                                                :connected (:success colors)
                                                                :connecting (:warning colors)
                                                                (:error colors))}}
                                      {:fx/type :label
                                       :text "LM Studio"
                                       :style {:-fx-font-size 12
                                               :-fx-text-fill (:text-secondary colors)}}]}
                          {:fx/type :h-box
                           :spacing 8
                           :alignment :center-left
                           :children [{:fx/type :label
                                       :text "●"
                                       :style {:-fx-text-fill (case (:web-app (:connections @*state))
                                                                :connected (:success colors)
                                                                :connecting (:warning colors)
                                                                (:error colors))}}
                                      {:fx/type :label
                                       :text "Web App"
                                       :style {:-fx-font-size 12
                                               :-fx-text-fill (:text-secondary colors)}}]}
                          {:fx/type :h-box
                           :spacing 8
                           :alignment :center-left
                           :children [{:fx/type :label
                                       :text "●"
                                       :style {:-fx-text-fill (case (:slack (:connections @*state))
                                                                :connected (:success colors)
                                                                :connecting (:warning colors)
                                                                (:error colors))}}
                                      {:fx/type :label
                                       :text "Slack"
                                       :style {:-fx-font-size 12
                                               :-fx-text-fill (:text-secondary colors)}}]}]}]})

;; =============================================================================
;; Dashboard View
;; =============================================================================

(defn stat-card [{:keys [icon label value color]}]
  {:fx/type :v-box
   :style {:-fx-background-color (:bg-card colors)
           :-fx-background-radius 8
           :-fx-border-color (:border colors)
           :-fx-border-radius 8}
   :padding 20
   :spacing 8
   :h-box/hgrow :always
   :children [{:fx/type :h-box
               :alignment :center-left
               :spacing 8
               :children [{:fx/type :label
                           :text icon
                           :style {:-fx-font-size 20}}
                          {:fx/type :label
                           :text label
                           :style {:-fx-font-size 12
                                   :-fx-text-fill (:text-secondary colors)}}]}
              {:fx/type :label
               :text (str value)
               :style {:-fx-font-size 32
                       :-fx-font-weight :bold
                       :-fx-text-fill (or color (:text-primary colors))}}]})

(defn dashboard-view [{:keys [stats scan updates]}]
  {:fx/type :scroll-pane
   :fit-to-width true
   :style {:-fx-background-color (:bg-primary colors)}
   :content
   {:fx/type :v-box
    :padding 24
    :spacing 24
    :children [;; Header
               {:fx/type :h-box
                :alignment :center-left
                :children [{:fx/type :v-box
                            :h-box/hgrow :always
                            :children [{:fx/type :label
                                        :text "Dashboard"
                                        :style {:-fx-font-size 24
                                                :-fx-font-weight :bold
                                                :-fx-text-fill (:text-primary colors)}}
                                       {:fx/type :h-box
                                        :spacing 12
                                        :alignment :center-left
                                        :children [{:fx/type :label
                                                    :text (str "Last sync: " (or (:last-sync stats) "Never"))
                                                    :style {:-fx-font-size 12
                                                            :-fx-text-fill (:text-muted colors)}}
                                                   {:fx/type :label
                                                    :text "•"
                                                    :style {:-fx-text-fill (:text-muted colors)}}
                                                   {:fx/type :label
                                                    :text (str "Code updated: " 
                                                               (if-let [updated (:last-updated updates)]
                                                                 (str updated 
                                                                      (when (> (:outdated-days updates) 0)
                                                                        (str " (" (:outdated-days updates) " days ago)")))
                                                                 "Never"))
                                                    :style {:-fx-font-size 12
                                                            :-fx-text-fill (if (> (or (:outdated-days updates) 0) 7)
                                                                             (:warning colors)
                                                                             (:text-muted colors))}}
                                                   (when (:available updates)
                                                     {:fx/type :label
                                                      :text "⚠️ Update available!"
                                                      :style {:-fx-font-size 12
                                                              :-fx-font-weight :bold
                                                              :-fx-text-fill (:warning colors)}})]}]}
                           ;; Update button
                           {:fx/type :button
                            :text (cond
                                    (:checking updates) "Checking..."
                                    (:available updates) "Update Now"
                                    :else "Check for Updates")
                            :disable (:checking updates)
                            :on-action {:event/type ::check-updates}
                            :style {:-fx-background-color (if (:available updates)
                                                            (:success colors)
                                                            (:accent colors))
                                    :-fx-text-fill :white
                                    :-fx-background-radius 6
                                    :-fx-padding "8 16"}}]}
               
               ;; Stats Grid
               {:fx/type :h-box
                :spacing 16
                :children [(stat-card {:icon "📄" :label "Files Scanned" :value (:files-scanned stats)})
                           (stat-card {:icon "🧠" :label "Models Found" :value (:models-found stats) :color (:accent colors)})
                           (stat-card {:icon "📊" :label "Analyzed" :value (:documents-analyzed stats)})
                           (stat-card {:icon "🎯" :label "Lollapalooza" :value (:lollapalooza-events stats) :color (:purple colors)})]}
               
               ;; Current Activity
               {:fx/type :v-box
                :style {:-fx-background-color (:bg-card colors)
                        :-fx-background-radius 8
                        :-fx-border-color (:border colors)
                        :-fx-border-radius 8}
                :padding 20
                :spacing 12
                :children [{:fx/type :label
                            :text "Current Activity"
                            :style {:-fx-font-size 16
                                    :-fx-font-weight :bold
                                    :-fx-text-fill (:text-primary colors)}}
                           {:fx/type :h-box
                            :spacing 12
                            :alignment :center-left
                            :children [{:fx/type :label
                                        :text (case (:status scan)
                                                :idle "⏸"
                                                :scanning "🔄"
                                                :watching "👁"
                                                :paused "⏸")
                                        :style {:-fx-font-size 24}}
                                       {:fx/type :v-box
                                        :h-box/hgrow :always
                                        :children [{:fx/type :label
                                                    :text (case (:status scan)
                                                            :idle "Idle"
                                                            :scanning "Scanning..."
                                                            :watching "Watching for changes"
                                                            :paused "Paused")
                                                    :style {:-fx-font-size 14
                                                            :-fx-text-fill (:text-primary colors)}}
                                                   {:fx/type :label
                                                    :text (or (:current-file scan) "No file being processed")
                                                    :style {:-fx-font-size 12
                                                            :-fx-text-fill (:text-muted colors)}}]}]}
                           ;; Progress bar
                           (when (= (:status scan) :scanning)
                             {:fx/type :progress-bar
                              :progress (if (pos? (:total-files scan))
                                          (/ (:progress scan) (:total-files scan))
                                          0)
                              :max-width Double/MAX_VALUE
                              :style {:-fx-accent (:accent colors)}})]}
               
               ;; Recent Models Found
               {:fx/type :v-box
                :style {:-fx-background-color (:bg-card colors)
                        :-fx-background-radius 8
                        :-fx-border-color (:border colors)
                        :-fx-border-radius 8}
                :padding 20
                :spacing 12
                :children [{:fx/type :label
                            :text "Recent Models Found"
                            :style {:-fx-font-size 16
                                    :-fx-font-weight :bold
                                    :-fx-text-fill (:text-primary colors)}}
                           {:fx/type :flow-pane
                            :hgap 8
                            :vgap 8
                            :children (for [model (take 10 (:found-models scan))]
                                        {:fx/type :label
                                         :text (:name model)
                                         :style {:-fx-background-color (:bg-tertiary colors)
                                                 :-fx-text-fill (:text-primary colors)
                                                 :-fx-padding "4 8"
                                                 :-fx-background-radius 4
                                                 :-fx-font-size 12}})}]}]}})

;; =============================================================================
;; Scan View
;; =============================================================================

(defn scan-view [{:keys [scan watched-folders]}]
  {:fx/type :v-box
   :padding 24
   :spacing 24
   :style {:-fx-background-color (:bg-primary colors)}
   :children [;; Header
              {:fx/type :label
               :text "Scan Folder"
               :style {:-fx-font-size 24
                       :-fx-font-weight :bold
                       :-fx-text-fill (:text-primary colors)}}
              
              ;; Folder Selection
              {:fx/type :v-box
               :style {:-fx-background-color (:bg-card colors)
                        :-fx-background-radius 8
                        :-fx-border-color (:border colors)
                        :-fx-border-radius 8}
               :padding 20
               :spacing 16
               :children [{:fx/type :label
                           :text "Select a folder to scan for mental models"
                           :style {:-fx-font-size 14
                                   :-fx-text-fill (:text-secondary colors)}}
                          {:fx/type :h-box
                           :spacing 12
                           :children [{:fx/type :button
                                       :text "📁 Choose Folder"
                                       :on-action {:event/type ::choose-folder}
                                       :style {:-fx-background-color (:accent colors)
                                               :-fx-text-fill :white
                                               :-fx-background-radius 6
                                               :-fx-padding "12 24"
                                               :-fx-font-size 14}}
                                      {:fx/type :button
                                       :text "🔍 Scan Now"
                                       :disable (or (empty? watched-folders)
                                                    (= (:status scan) :scanning))
                                       :on-action {:event/type ::start-scan}
                                       :style {:-fx-background-color (:success colors)
                                               :-fx-text-fill :white
                                               :-fx-background-radius 6
                                               :-fx-padding "12 24"
                                               :-fx-font-size 14}}
                                      (when (= (:status scan) :scanning)
                                        {:fx/type :button
                                         :text "⏹ Stop"
                                         :on-action {:event/type ::stop-scan}
                                         :style {:-fx-background-color (:error colors)
                                                 :-fx-text-fill :white
                                                 :-fx-background-radius 6
                                                 :-fx-padding "12 24"
                                                 :-fx-font-size 14}})]}]}
              
              ;; Selected Folders
              {:fx/type :v-box
               :style {:-fx-background-color (:bg-card colors)
                        :-fx-background-radius 8
                        :-fx-border-color (:border colors)
                        :-fx-border-radius 8}
               :padding 20
               :spacing 12
               :children [{:fx/type :label
                           :text "Selected Folders"
                           :style {:-fx-font-size 16
                                   :-fx-font-weight :bold
                                   :-fx-text-fill (:text-primary colors)}}
                          (if (empty? watched-folders)
                            {:fx/type :label
                             :text "No folders selected. Click 'Choose Folder' to add one."
                             :style {:-fx-text-fill (:text-muted colors)}}
                            {:fx/type :v-box
                             :spacing 8
                             :children (for [folder watched-folders]
                                         {:fx/type :h-box
                                          :alignment :center-left
                                          :spacing 12
                                          :children [{:fx/type :label
                                                      :text "📁"
                                                      :style {:-fx-font-size 16}}
                                                     {:fx/type :label
                                                      :text folder
                                                      :h-box/hgrow :always
                                                      :style {:-fx-text-fill (:text-primary colors)}}
                                                     {:fx/type :button
                                                      :text "✕"
                                                      :on-action {:event/type ::remove-folder :folder folder}
                                                      :style {:-fx-background-color :transparent
                                                              :-fx-text-fill (:text-muted colors)}}]})})]}
              
              ;; Scan Progress
              (when (= (:status scan) :scanning)
                {:fx/type :v-box
                 :style {:-fx-background-color (:bg-card colors)
                          :-fx-background-radius 8
                          :-fx-border-color (:border colors)
                          :-fx-border-radius 8}
                 :padding 20
                 :spacing 12
                 :children [{:fx/type :h-box
                             :alignment :center-left
                             :children [{:fx/type :label
                                         :text "Scanning..."
                                         :h-box/hgrow :always
                                         :style {:-fx-font-size 16
                                                 :-fx-font-weight :bold
                                                 :-fx-text-fill (:text-primary colors)}}
                                        {:fx/type :label
                                         :text (str (:progress scan) " / " (:total-files scan) " files")
                                         :style {:-fx-text-fill (:text-secondary colors)}}]}
                            {:fx/type :progress-bar
                             :progress (if (pos? (:total-files scan))
                                         (/ (:progress scan) (:total-files scan))
                                         0)
                             :max-width Double/MAX_VALUE}
                            {:fx/type :label
                             :text (str "Current: " (or (:current-file scan) "..."))
                             :style {:-fx-text-fill (:text-muted colors)
                                     :-fx-font-size 12}}]})]})

;; =============================================================================
;; Watch View
;; =============================================================================

(defn watch-view [{:keys [scan watched-folders]}]
  {:fx/type :v-box
   :padding 24
   :spacing 24
   :style {:-fx-background-color (:bg-primary colors)}
   :children [{:fx/type :label
               :text "Watch Mode"
               :style {:-fx-font-size 24
                       :-fx-font-weight :bold
                       :-fx-text-fill (:text-primary colors)}}
              
              {:fx/type :v-box
               :style {:-fx-background-color (:bg-card colors)
                        :-fx-background-radius 8
                        :-fx-border-color (:border colors)
                        :-fx-border-radius 8}
               :padding 20
               :spacing 16
               :children [{:fx/type :label
                           :text "Continuously monitor folders for new or changed files"
                           :style {:-fx-font-size 14
                                   :-fx-text-fill (:text-secondary colors)}}
                          {:fx/type :h-box
                           :spacing 12
                           :children [(if (= (:status scan) :watching)
                                        {:fx/type :button
                                         :text "⏹ Stop Watching"
                                         :on-action {:event/type ::stop-watch}
                                         :style {:-fx-background-color (:error colors)
                                                 :-fx-text-fill :white
                                                 :-fx-background-radius 6
                                                 :-fx-padding "12 24"
                                                 :-fx-font-size 14}}
                                        {:fx/type :button
                                         :text "👁 Start Watching"
                                         :disable (empty? watched-folders)
                                         :on-action {:event/type ::start-watch}
                                         :style {:-fx-background-color (:success colors)
                                                 :-fx-text-fill :white
                                                 :-fx-background-radius 6
                                                 :-fx-padding "12 24"
                                                 :-fx-font-size 14}})]}
                          
                          (when (= (:status scan) :watching)
                            {:fx/type :h-box
                             :spacing 8
                             :alignment :center-left
                             :children [{:fx/type :label
                                         :text "●"
                                         :style {:-fx-text-fill (:success colors)
                                                 :-fx-font-size 20}}
                                        {:fx/type :label
                                         :text "Watching for changes..."
                                         :style {:-fx-text-fill (:success colors)}}]})]}]})

;; =============================================================================
;; Logs View
;; =============================================================================

(defn log-entry [{:keys [timestamp level message details]}]
  {:fx/type :h-box
   :spacing 12
   :padding {:top 8 :bottom 8 :left 12 :right 12}
   :style {:-fx-border-color (:border colors)
           :-fx-border-width "0 0 1 0"}
   :children [{:fx/type :label
               :text (case level
                       :info "ℹ️"
                       :warning "⚠️"
                       :error "❌"
                       :model "🧠"
                       :success "✅"
                       "📝")
               :style {:-fx-font-size 14}}
              {:fx/type :label
               :text (.toString timestamp)
               :min-width 180
               :style {:-fx-text-fill (:text-muted colors)
                       :-fx-font-size 11
                       :-fx-font-family "monospace"}}
              {:fx/type :label
               :text message
               :h-box/hgrow :always
               :style {:-fx-text-fill (:text-primary colors)}}]})

(defn logs-view [{:keys [logs log-filter]}]
  {:fx/type :v-box
   :padding 24
   :spacing 16
   :style {:-fx-background-color (:bg-primary colors)}
   :v-box/vgrow :always
   :children [{:fx/type :h-box
               :alignment :center-left
               :children [{:fx/type :label
                           :text "Live Logs"
                           :h-box/hgrow :always
                           :style {:-fx-font-size 24
                                   :-fx-font-weight :bold
                                   :-fx-text-fill (:text-primary colors)}}
                          {:fx/type :button
                           :text "Clear"
                           :on-action {:event/type ::clear-logs}
                           :style {:-fx-background-color (:bg-tertiary colors)
                                   :-fx-text-fill (:text-primary colors)
                                   :-fx-background-radius 6}}]}
              
              ;; Filter buttons
              {:fx/type :h-box
               :spacing 8
               :children (for [[filter-key label] [[:all "All"] [:info "Info"] [:warning "Warnings"] [:error "Errors"] [:model "Models"]]]
                           {:fx/type :button
                            :text label
                            :on-action {:event/type ::set-log-filter :filter filter-key}
                            :style {:-fx-background-color (if (= log-filter filter-key)
                                                            (:accent colors)
                                                            (:bg-tertiary colors))
                                    :-fx-text-fill :white
                                    :-fx-background-radius 4
                                    :-fx-padding "6 12"}})}
              
              ;; Log list
              {:fx/type :scroll-pane
               :fit-to-width true
               :v-box/vgrow :always
               :style {:-fx-background-color (:bg-card colors)}
               :content
               {:fx/type :v-box
                :children (for [log (reverse (take 100 (if (= log-filter :all)
                                                         logs
                                                         (filter #(= (:level %) log-filter) logs))))]
                            (log-entry log))}}]})

;; =============================================================================
;; Settings View
;; =============================================================================

(defn settings-input [{:keys [label value placeholder on-change]}]
  {:fx/type :v-box
   :spacing 6
   :children [{:fx/type :label
               :text label
               :style {:-fx-text-fill (:text-secondary colors)
                       :-fx-font-size 12}}
              {:fx/type :text-field
               :text (or value "")
               :prompt-text placeholder
               :on-text-changed on-change
               :style {:-fx-background-color (:bg-tertiary colors)
                       :-fx-text-fill (:text-primary colors)
                       :-fx-border-color (:border colors)
                       :-fx-border-radius 4
                       :-fx-background-radius 4
                       :-fx-padding 10}}]})

(defn settings-view [{:keys [settings updates]}]
  {:fx/type :scroll-pane
   :fit-to-width true
   :style {:-fx-background-color (:bg-primary colors)}
   :content
   {:fx/type :v-box
    :padding 24
    :spacing 24
    :children [{:fx/type :label
                :text "Settings"
                :style {:-fx-font-size 24
                        :-fx-font-weight :bold
                        :-fx-text-fill (:text-primary colors)}}
               
               ;; LM Studio Settings
               {:fx/type :v-box
                :style {:-fx-background-color (:bg-card colors)
                         :-fx-background-radius 8
                         :-fx-border-color (:border colors)
                         :-fx-border-radius 8}
                :padding 20
                :spacing 16
                :children [{:fx/type :label
                            :text "LM Studio Connection"
                            :style {:-fx-font-size 16
                                    :-fx-font-weight :bold
                                    :-fx-text-fill (:text-primary colors)}}
                           (settings-input {:label "LM Studio URL"
                                            :value (:lm-studio-url settings)
                                            :placeholder "http://localhost:1234"
                                            :on-change {:event/type ::update-setting :key :lm-studio-url}})
                           {:fx/type :button
                            :text "Test Connection"
                            :on-action {:event/type ::test-lm-studio}
                            :style {:-fx-background-color (:accent colors)
                                    :-fx-text-fill :white
                                    :-fx-background-radius 6}}]}
               
               ;; Web App Sync
               {:fx/type :v-box
                :style {:-fx-background-color (:bg-card colors)
                         :-fx-background-radius 8
                         :-fx-border-color (:border colors)
                         :-fx-border-radius 8}
                :padding 20
                :spacing 16
                :children [{:fx/type :label
                            :text "Web App Sync"
                            :style {:-fx-font-size 16
                                    :-fx-font-weight :bold
                                    :-fx-text-fill (:text-primary colors)}}
                           (settings-input {:label "Web App URL"
                                            :value (:web-app-url settings)
                                            :placeholder "https://your-app.manus.space"
                                            :on-change {:event/type ::update-setting :key :web-app-url}})]}
               
               ;; Slack Integration
               {:fx/type :v-box
                :style {:-fx-background-color (:bg-card colors)
                         :-fx-background-radius 8
                         :-fx-border-color (:border colors)
                         :-fx-border-radius 8}
                :padding 20
                :spacing 16
                :children [{:fx/type :label
                            :text "Slack Integration"
                            :style {:-fx-font-size 16
                                    :-fx-font-weight :bold
                                    :-fx-text-fill (:text-primary colors)}}
                           (settings-input {:label "Slack Webhook URL"
                                            :value (:slack-webhook settings)
                                            :placeholder "https://hooks.slack.com/services/..."
                                            :on-change {:event/type ::update-setting :key :slack-webhook}})
                           (settings-input {:label "Slack Channel ID (for remote commands)"
                                            :value (:slack-channel settings)
                                            :placeholder "C0123456789"
                                            :on-change {:event/type ::update-setting :key :slack-channel}})]}
               
               ;; Updates
               {:fx/type :v-box
                :style {:-fx-background-color (:bg-card colors)
                         :-fx-background-radius 8
                         :-fx-border-color (:border colors)
                         :-fx-border-radius 8}
                :padding 20
                :spacing 16
                :children [{:fx/type :h-box
                            :alignment :center-left
                            :children [{:fx/type :label
                                        :text "Updates"
                                        :h-box/hgrow :always
                                        :style {:-fx-font-size 16
                                                :-fx-font-weight :bold
                                                :-fx-text-fill (:text-primary colors)}}
                                       {:fx/type :button
                                        :text (if (:checking updates) "Checking..." "Check Now")
                                        :disable (:checking updates)
                                        :on-action {:event/type ::check-updates}
                                        :style {:-fx-background-color (:accent colors)
                                                :-fx-text-fill :white
                                                :-fx-background-radius 6}}]}
                           {:fx/type :h-box
                            :spacing 24
                            :children [{:fx/type :v-box
                                        :children [{:fx/type :label
                                                    :text "Current Version"
                                                    :style {:-fx-text-fill (:text-muted colors)
                                                            :-fx-font-size 12}}
                                                   {:fx/type :label
                                                    :text (:current-version updates)
                                                    :style {:-fx-text-fill (:text-primary colors)}}]}
                                       {:fx/type :v-box
                                        :children [{:fx/type :label
                                                    :text "Last Checked"
                                                    :style {:-fx-text-fill (:text-muted colors)
                                                            :-fx-font-size 12}}
                                                   {:fx/type :label
                                                    :text (or (:last-check updates) "Never")
                                                    :style {:-fx-text-fill (:text-primary colors)}}]}]}
                           {:fx/type :check-box
                            :text "Auto-update every 5 minutes"
                            :selected (:auto-update settings)
                            :on-selected-changed {:event/type ::toggle-auto-update}}
                           {:fx/type :check-box
                            :text "Listen for Slack update commands"
                            :selected (not (str/blank? (:slack-channel settings)))}]}]}})

;; =============================================================================
;; Main Window
;; =============================================================================

(defn root-view [{:keys [current-view stats scan watched-folders logs log-filter settings updates]}]
  {:fx/type :stage
   :showing true
   :title "Mental Models Desktop"
   :width 1200
   :height 800
   :scene {:fx/type :scene
           :stylesheets [(::css (meta *state))]
           :root {:fx/type :h-box
                  :style {:-fx-background-color (:bg-primary colors)}
                  :children [(sidebar {:current-view current-view})
                             {:fx/type :v-box
                              :h-box/hgrow :always
                              :children [(case current-view
                                           :dashboard (dashboard-view {:stats stats :scan scan :updates updates})
                                           :scan (scan-view {:scan scan :watched-folders watched-folders})
                                           :watch (watch-view {:scan scan :watched-folders watched-folders})
                                           :logs (logs-view {:logs logs :log-filter log-filter})
                                           :settings (settings-view {:settings settings :updates updates})
                                           (dashboard-view {:stats stats :scan scan :updates updates}))]}]}}})

;; =============================================================================
;; Event Handlers
;; =============================================================================

(defmulti event-handler :event/type)

(defmethod event-handler ::navigate [{:keys [view]}]
  (swap! *state assoc :current-view view))

(defmethod event-handler ::choose-folder [_]
  (let [chooser (DirectoryChooser.)]
    (.setTitle chooser "Select Folder to Scan")
    (when-let [dir (.showDialog chooser nil)]
      (swap! *state update :watched-folders conj (.getAbsolutePath dir))
      (add-log! :info (str "Added folder: " (.getAbsolutePath dir))))))

(defmethod event-handler ::remove-folder [{:keys [folder]}]
  (swap! *state update :watched-folders #(vec (remove #{folder} %)))
  (add-log! :info (str "Removed folder: " folder)))

(defmethod event-handler ::start-scan [_]
  (swap! *state assoc-in [:scan :status] :scanning)
  (add-log! :info "Starting scan..."))

(defmethod event-handler ::stop-scan [_]
  (swap! *state assoc-in [:scan :status] :idle)
  (add-log! :info "Scan stopped"))

(defmethod event-handler ::start-watch [_]
  (swap! *state assoc-in [:scan :status] :watching)
  (add-log! :info "Watch mode started"))

(defmethod event-handler ::stop-watch [_]
  (swap! *state assoc-in [:scan :status] :idle)
  (add-log! :info "Watch mode stopped"))

(defmethod event-handler ::check-updates [_]
  (swap! *state assoc-in [:updates :checking] true)
  (add-log! :info "Checking for updates...")
  ;; Simulate update check
  (future
    (Thread/sleep 2000)
    (swap! *state assoc-in [:updates :checking] false)
    (swap! *state assoc-in [:updates :last-check] (str (java.time.LocalDateTime/now)))
    (add-log! :success "Update check complete")))

(defmethod event-handler ::clear-logs [_]
  (swap! *state assoc :logs []))

(defmethod event-handler ::set-log-filter [{:keys [filter]}]
  (swap! *state assoc :log-filter filter))

(defmethod event-handler ::update-setting [{:keys [key fx/event]}]
  (swap! *state assoc-in [:settings key] event))

(defmethod event-handler ::toggle-auto-update [{:keys [fx/event]}]
  (swap! *state assoc-in [:settings :auto-update] event))

(defmethod event-handler ::test-lm-studio [_]
  (add-log! :info "Testing LM Studio connection...")
  (set-connection! :lm-studio :connecting)
  ;; Actual connection test would go here
  (future
    (Thread/sleep 1000)
    (set-connection! :lm-studio :connected)
    (add-log! :success "LM Studio connected!")))

(defmethod event-handler :default [event]
  (println "Unhandled event:" event))

;; =============================================================================
;; Application Entry Point
;; =============================================================================

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root-view)
   :opts {:fx.opt/map-event-handler event-handler}))

(defn -main [& args]
  (Platform/setImplicitExit true)
  (add-log! :info "Mental Models Desktop started")
  (add-log! :info "Checking connections...")
  
  ;; Start the renderer
  (fx/mount-renderer *state renderer)
  
  ;; Initial connection checks
  (future
    (Thread/sleep 500)
    (set-connection! :lm-studio :connecting)
    (Thread/sleep 1000)
    (set-connection! :lm-studio :connected)
    (add-log! :success "LM Studio connected")))
