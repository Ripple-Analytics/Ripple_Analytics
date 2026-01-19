# DEVIN TASK: Windows Desktop App (CLJFX/Electric Clojure)

## PRIORITY: HIGH
## BRANCH: `feature/windows-desktop-app`

---

## Overview

Build a native Windows desktop application using **CLJFX** (Clojure wrapper for JavaFX) that integrates with the Mental Models System. This app will:

1. **Watch folders** for new documents (txt, md, pdf, docx)
2. **Analyze text** using the mental model detection algorithms
3. **Display results** in real-time with Electric Clojure reactive updates
4. **Sync with web app** via API
5. **Run in system tray** for background operation

---

## Tech Stack

- **Language**: Clojure/ClojureScript
- **UI Framework**: CLJFX (JavaFX wrapper)
- **Reactive Layer**: Electric Clojure
- **File Watching**: Java WatchService (via juxt/dirwatch)
- **PDF Parsing**: Apache PDFBox 3.0.1
- **Word/Excel**: Apache POI 5.2.5
- **HTTP Client**: clj-http
- **Database**: SQLite (local cache) + MySQL (sync to server)
- **Packaging**: jpackage (native .msi installer)

---

## File Structure

```
src/mental_models/desktop/
├── core.clj           # Main entry point, app lifecycle
├── ui/
│   ├── main_window.clj    # Main application window
│   ├── dashboard.clj      # Dashboard view with stats
│   ├── analysis.clj       # Analysis results view
│   ├── settings.clj       # Settings panel
│   ├── system_tray.clj    # System tray integration
│   └── styles.clj         # CSS-like styling
├── services/
│   ├── file_watcher.clj   # Folder watching service
│   ├── analyzer.clj       # Mental model analysis
│   ├── sync.clj           # API sync with web app
│   └── cache.clj          # Local SQLite cache
├── utils/
│   ├── file_reader.clj    # Read various file formats
│   └── config.clj         # App configuration
└── resources/
    ├── icon.ico           # App icon
    └── styles.css         # JavaFX styles
```

---

## Core Requirements

### 1. Main Window (CLJFX)

```clojure
(ns mental-models.desktop.ui.main-window
  (:require [cljfx.api :as fx]
            [mental-models.desktop.ui.dashboard :as dashboard]
            [mental-models.desktop.ui.analysis :as analysis]
            [mental-models.desktop.ui.settings :as settings]))

(defn main-view [{:keys [fx/context]}]
  {:fx/type :stage
   :showing true
   :title "Mental Models System"
   :width 1200
   :height 800
   :scene {:fx/type :scene
           :stylesheets ["styles.css"]
           :root {:fx/type :border-pane
                  :top {:fx/type :menu-bar
                        :menus [{:fx/type :menu
                                 :text "File"
                                 :items [{:fx/type :menu-item
                                          :text "Add Watch Folder..."
                                          :on-action {:event/type ::add-folder}}
                                         {:fx/type :menu-item
                                          :text "Analyze File..."
                                          :on-action {:event/type ::analyze-file}}
                                         {:fx/type :separator-menu-item}
                                         {:fx/type :menu-item
                                          :text "Exit"
                                          :on-action {:event/type ::exit}}]}]}
                  :left {:fx/type sidebar-view}
                  :center {:fx/type :tab-pane
                           :tabs [{:fx/type :tab
                                   :text "Dashboard"
                                   :closable false
                                   :content {:fx/type dashboard/view}}
                                  {:fx/type :tab
                                   :text "Analysis"
                                   :closable false
                                   :content {:fx/type analysis/view}}
                                  {:fx/type :tab
                                   :text "Settings"
                                   :closable false
                                   :content {:fx/type settings/view}}]}}}})
```

### 2. Folder Watcher (Java WatchService)

```clojure
(ns mental-models.desktop.services.file-watcher
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go-loop <! >! chan]])
  (:import [java.nio.file FileSystems Paths StandardWatchEventKinds WatchService]))

(defn create-watcher
  "Create a file watcher for the given directories."
  [directories on-file-created]
  (let [watcher (.newWatchService (FileSystems/getDefault))]
    (doseq [dir directories]
      (let [path (Paths/get dir (into-array String []))]
        (.register path watcher
                   (into-array [StandardWatchEventKinds/ENTRY_CREATE
                                StandardWatchEventKinds/ENTRY_MODIFY]))))
    (go-loop []
      (when-let [key (.take watcher)]
        (doseq [event (.pollEvents key)]
          (let [kind (.kind event)
                filename (str (.context event))]
            (when (#{StandardWatchEventKinds/ENTRY_CREATE
                     StandardWatchEventKinds/ENTRY_MODIFY} kind)
              (on-file-created filename))))
        (.reset key)
        (recur)))
    watcher))

(defn watch-folders!
  "Start watching configured folders for new files."
  [state]
  (let [folders (get-in @state [:settings :watch-folders])
        on-file (fn [f] (analyze-file! state f))]
    (create-watcher folders on-file)))
```

### 3. File Reader (Multi-format)

```clojure
(ns mental-models.desktop.utils.file-reader
  (:require [clojure.java.io :as io])
  (:import [org.apache.pdfbox Loader]
           [org.apache.pdfbox.text PDFTextStripper]
           [org.apache.poi.xwpf.usermodel XWPFDocument]
           [org.apache.poi.xssf.usermodel XSSFWorkbook]))

(defmulti read-file
  "Read text content from various file formats."
  (fn [file] (-> file .getName (.split "\\.") last .toLowerCase)))

(defmethod read-file "txt" [file]
  (slurp file))

(defmethod read-file "md" [file]
  (slurp file))

(defmethod read-file "pdf" [file]
  (with-open [doc (Loader/loadPDF file)]
    (let [stripper (PDFTextStripper.)]
      (.getText stripper doc))))

(defmethod read-file "docx" [file]
  (with-open [doc (XWPFDocument. (io/input-stream file))]
    (->> (.getParagraphs doc)
         (map #(.getText %))
         (clojure.string/join "\n"))))

(defmethod read-file "xlsx" [file]
  (with-open [wb (XSSFWorkbook. (io/input-stream file))]
    (->> (for [sheet (iterator-seq (.sheetIterator wb))
               row (iterator-seq (.rowIterator sheet))
               cell (iterator-seq (.cellIterator row))]
           (.toString cell))
         (clojure.string/join " "))))

(defmethod read-file :default [file]
  (slurp file))
```

### 4. Mental Model Analyzer Integration

```clojure
(ns mental-models.desktop.services.analyzer
  (:require [mental-models.models.unified-detector :as detector]
            [mental-models.services.lm-studio :as lm]
            [clojure.core.async :as async]))

(defn analyze-text
  "Analyze text against all 200+ mental models."
  [text {:keys [use-llm? progress-fn]}]
  (let [;; Step 1: Pattern matching (fast)
        pattern-results (detector/detect-all-patterns text)
        _ (when progress-fn (progress-fn 0.3 "Pattern matching complete"))
        
        ;; Step 2: Statistical analysis
        stat-results (detector/statistical-analysis text)
        _ (when progress-fn (progress-fn 0.5 "Statistical analysis complete"))
        
        ;; Step 3: LLM semantic analysis (if enabled and LM Studio running)
        llm-results (when (and use-llm? (lm/connected?))
                      (lm/batch-analyze text (detector/get-top-candidates pattern-results 20)))
        _ (when progress-fn (progress-fn 0.8 "LLM analysis complete"))
        
        ;; Step 4: Combine and score
        final-results (detector/combine-results pattern-results stat-results llm-results)]
    
    (when progress-fn (progress-fn 1.0 "Analysis complete"))
    
    {:detected-models (:top-models final-results)
     :all-scores (:all-scores final-results)
     :lollapalooza (:lollapalooza-detected final-results)
     :failure-modes (:failure-modes final-results)
     :processing-time-ms (:duration final-results)}))

(defn analyze-file!
  "Analyze a file and update state."
  [state file-path]
  (let [text (file-reader/read-file (io/file file-path))
        results (analyze-text text {:use-llm? true
                                    :progress-fn (fn [p msg]
                                                   (swap! state assoc-in 
                                                          [:analysis :progress] 
                                                          {:percent p :message msg}))})]
    (swap! state update :analyses conj
           {:file file-path
            :timestamp (System/currentTimeMillis)
            :results results})
    results))
```

### 5. System Tray Integration

```clojure
(ns mental-models.desktop.ui.system-tray
  (:import [java.awt SystemTray TrayIcon PopupMenu MenuItem]
           [java.awt.event ActionListener]
           [javax.imageio ImageIO]))

(defn create-tray-icon
  "Create system tray icon with menu."
  [on-show on-exit]
  (when (SystemTray/isSupported)
    (let [tray (SystemTray/getSystemTray)
          image (ImageIO/read (io/resource "icon.png"))
          popup (doto (PopupMenu.)
                  (.add (doto (MenuItem. "Show")
                          (.addActionListener
                           (reify ActionListener
                             (actionPerformed [_ _] (on-show))))))
                  (.add (doto (MenuItem. "Exit")
                          (.addActionListener
                           (reify ActionListener
                             (actionPerformed [_ _] (on-exit)))))))
          icon (TrayIcon. image "Mental Models" popup)]
      (.setImageAutoSize icon true)
      (.add tray icon)
      icon)))
```

### 6. API Sync with Web App

```clojure
(ns mental-models.desktop.services.sync
  (:require [clj-http.client :as http]
            [jsonista.core :as json]
            [environ.core :refer [env]]))

(def api-base (or (env :api-url) "https://your-web-app.manus.space/api"))

(defn sync-analysis!
  "Sync analysis results to web app."
  [analysis]
  (try
    (http/post (str api-base "/v1/analyze")
               {:headers {"Authorization" (str "Bearer " (env :api-token))
                          "Content-Type" "application/json"}
                :body (json/write-value-as-string analysis)
                :socket-timeout 5000
                :connection-timeout 5000})
    {:success true}
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn fetch-models!
  "Fetch latest mental models from web app."
  []
  (try
    (let [response (http/get (str api-base "/v1/models")
                             {:headers {"Authorization" (str "Bearer " (env :api-token))}
                              :as :json})]
      (:body response))
    (catch Exception e
      {:error (.getMessage e)})))

(defn sync-loop!
  "Background sync loop."
  [state interval-ms]
  (future
    (while true
      (try
        ;; Push pending analyses
        (doseq [analysis (get-in @state [:pending-sync])]
          (when (:success (sync-analysis! analysis))
            (swap! state update :pending-sync #(remove #{analysis} %))))
        ;; Pull latest models
        (when-let [models (fetch-models!)]
          (swap! state assoc :models models))
        (catch Exception _))
      (Thread/sleep interval-ms))))
```

---

## UI Design Requirements

### Dashboard View
- **Total documents analyzed** (clickable → shows list)
- **Models detected today** (clickable → shows breakdown)
- **Lollapalooza events** (highlighted, clickable)
- **Active failure modes** (warning indicators)
- **Watch folder status** (green/red indicators)
- **LM Studio connection status**

### Analysis View
- **File list** with analysis status
- **Model detection results** organized by Munger's hierarchy
- **Confidence scores** with visual bars
- **Evidence excerpts** from text
- **Failure mode warnings**

### Settings View
- **Watch folders** (add/remove)
- **LM Studio URL** configuration
- **API sync settings**
- **Analysis preferences** (LLM on/off, sensitivity)
- **Notification settings**

---

## Packaging for Windows

### jpackage Command

```bash
jpackage \
  --name "Mental Models System" \
  --app-version "1.0.0" \
  --vendor "Ripple Analytics" \
  --description "Mental Models Analysis Desktop App" \
  --icon resources/icon.ico \
  --type msi \
  --input target/classes \
  --main-jar mental-models.jar \
  --main-class mental_models.desktop.core \
  --java-options "-Xmx2g" \
  --win-menu \
  --win-shortcut \
  --win-dir-chooser
```

### Build Script (build.clj)

```clojure
(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'mental-models/desktop)
(def version "1.0.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"
                            :aliases [:desktop]}))
(def uber-file "target/mental-models.jar")

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'mental-models.desktop.core}))
```

---

## Acceptance Criteria

- [ ] App launches on Windows 10/11
- [ ] Can add/remove watch folders
- [ ] Detects new files in watched folders within 5 seconds
- [ ] Reads txt, md, pdf, docx files correctly
- [ ] Analyzes text against all mental models
- [ ] Displays results organized by Munger's hierarchy
- [ ] Shows Lollapalooza detection when 3+ models converge
- [ ] Syncs results to web app API
- [ ] Runs in system tray when minimized
- [ ] Connects to LM Studio for semantic analysis (optional)
- [ ] Builds to .msi installer with jpackage
- [ ] All numbers are real (no hardcoded fake data)
- [ ] Every count is clickable to show source

---

## Testing Requirements

```clojure
;; test/mental_models/desktop/core_test.clj

(deftest test-file-reading
  (testing "Reads various file formats"
    (is (string? (file-reader/read-file (io/file "test.txt"))))
    (is (string? (file-reader/read-file (io/file "test.pdf"))))
    (is (string? (file-reader/read-file (io/file "test.docx"))))))

(deftest test-analysis
  (testing "Analyzes text and returns results"
    (let [result (analyzer/analyze-text "Test text for analysis" {})]
      (is (map? result))
      (is (contains? result :detected-models))
      (is (contains? result :all-scores)))))

(deftest test-folder-watcher
  (testing "Detects new files"
    (let [detected (atom [])
          watcher (file-watcher/create-watcher 
                    ["test-folder"]
                    #(swap! detected conj %))]
      (spit "test-folder/new-file.txt" "test")
      (Thread/sleep 1000)
      (is (seq @detected)))))
```

---

## Environment Variables

```bash
# .env
LM_STUDIO_URL=http://localhost:1234/v1
API_URL=https://your-web-app.manus.space/api
API_TOKEN=your-api-token
DB_PATH=~/.mental-models/cache.db
LOG_LEVEL=info
```

---

## Timeline

- **Day 1-2**: Core CLJFX window, file watcher, file readers
- **Day 3-4**: Analysis integration, results display
- **Day 5**: API sync, system tray, settings
- **Day 6**: Testing, packaging, installer
- **Day 7**: Polish, documentation, release

---

## Notes

- Use the existing analysis algorithms in `src/mental_models/models/`
- Share code with the web app where possible
- All UI must follow Munger's organizing hierarchy
- Every number must be clickable to show data source
- No fake/hardcoded data - everything must be real
