(ns mental-models.pipeline.integration.result-formatter
  "Analysis Result Formatter
   
   Formats analysis results for different output formats:
   - JSON for API responses
   - HTML for web display
   - Markdown for reports
   - CSV for data export
   - Plain text for CLI output"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.logging.structured :as log]
   [clojure.string :as str]
   [clojure.data.json :as json]))

;; =============================================================================
;; FORMAT TEMPLATES
;; =============================================================================

(def confidence-levels
  {[0.0 0.3] {:label "Low" :color "#ff6b6b" :emoji "🔴"}
   [0.3 0.6] {:label "Medium" :color "#ffd93d" :emoji "🟡"}
   [0.6 0.8] {:label "High" :color "#6bcb77" :emoji "🟢"}
   [0.8 1.0] {:label "Very High" :color "#4d96ff" :emoji "🔵"}})

(defn get-confidence-level [confidence]
  (some (fn [[[low high] level]]
          (when (and (>= confidence low) (< confidence high))
            level))
        confidence-levels))

;; =============================================================================
;; JSON FORMATTER
;; =============================================================================

(defn format-json
  "Format analysis result as JSON."
  [result & {:keys [pretty] :or {pretty true}}]
  (let [formatted {:analysis-id (:id result (str (java.util.UUID/randomUUID)))
                   :timestamp (:timestamp result (System/currentTimeMillis))
                   :source {:file-path (:file-path result)
                            :text-length (:text-length result)}
                   :detections (map (fn [d]
                                      {:model-id (:model-id d)
                                       :model-name (:model-name d)
                                       :confidence (:confidence d)
                                       :confidence-level (:label (get-confidence-level (:confidence d)))
                                       :evidence (:evidence d)})
                                    (:detections result))
                   :lollapalooza (when-let [l (:lollapalooza result)]
                                   {:detected true
                                    :models (:models l)
                                    :combined-confidence (:combined-confidence l)
                                    :description (:description l)})
                   :summary {:total-models (count (:detections result))
                             :high-confidence (count (filter #(>= (:confidence %) 0.7) (:detections result)))
                             :has-lollapalooza (boolean (:lollapalooza result))}}]
    (if pretty
      (json/write-str formatted :indent true)
      (json/write-str formatted))))

;; =============================================================================
;; HTML FORMATTER
;; =============================================================================

(defn format-html
  "Format analysis result as HTML."
  [result]
  (let [detections (:detections result)
        lollapalooza (:lollapalooza result)]
    (str
     "<div class="analysis-result">
"
     "  <h2>Mental Model Analysis</h2>
"
     "  <p class="timestamp">Analyzed: " (java.util.Date. (:timestamp result (System/currentTimeMillis))) "</p>
"
     (when (:file-path result)
       (str "  <p class="source">Source: " (:file-path result) "</p>
"))
     "
"
     (when lollapalooza
       (str
        "  <div class="lollapalooza-alert">
"
        "    <h3>Lollapalooza Effect Detected!</h3>
"
        "    <p>" (:description lollapalooza) "</p>
"
        "    <p>Models: " (str/join ", " (map :model-name (:models lollapalooza))) "</p>
"
        "    <p>Combined Confidence: " (format "%.1f%%" (* 100 (:combined-confidence lollapalooza))) "</p>
"
        "  </div>

"))
     "  <h3>Detected Models (" (count detections) ")</h3>
"
     "  <table class="detections">
"
     "    <thead><tr><th>Model</th><th>Confidence</th><th>Evidence</th></tr></thead>
"
     "    <tbody>
"
     (str/join "
"
               (map (fn [d]
                      (let [level (get-confidence-level (:confidence d))]
                        (str "      <tr>
"
                             "        <td>" (:model-name d) "</td>
"
                             "        <td style="color: " (:color level) "">"
                             (format "%.1f%%" (* 100 (:confidence d)))
                             " (" (:label level) ")</td>
"
                             "        <td>" (or (:evidence d) "N/A") "</td>
"
                             "      </tr>")))
                    (sort-by :confidence > detections)))
     "
    </tbody>
"
     "  </table>
"
     "</div>")))

;; =============================================================================
;; MARKDOWN FORMATTER
;; =============================================================================

(defn format-markdown
  "Format analysis result as Markdown."
  [result]
  (let [detections (:detections result)
        lollapalooza (:lollapalooza result)]
    (str
     "# Mental Model Analysis

"
     "**Analyzed:** " (java.util.Date. (:timestamp result (System/currentTimeMillis))) "
"
     (when (:file-path result)
       (str "**Source:** `" (:file-path result) "`
"))
     "
"
     (when lollapalooza
       (str
        "## Lollapalooza Effect Detected!

"
        "> " (:description lollapalooza) "

"
        "**Models:** " (str/join ", " (map :model-name (:models lollapalooza))) "
"
        "**Combined Confidence:** " (format "%.1f%%" (* 100 (:combined-confidence lollapalooza))) "

"))
     "## Detected Models (" (count detections) ")

"
     "| Model | Confidence | Level | Evidence |
"
     "|-------|------------|-------|----------|
"
     (str/join "
"
               (map (fn [d]
                      (let [level (get-confidence-level (:confidence d))]
                        (str "| " (:model-name d)
                             " | " (format "%.1f%%" (* 100 (:confidence d)))
                             " | " (:emoji level) " " (:label level)
                             " | " (or (:evidence d) "N/A") " |")))
                    (sort-by :confidence > detections)))
     "
")))

;; =============================================================================
;; CSV FORMATTER
;; =============================================================================

(defn format-csv
  "Format analysis result as CSV."
  [result]
  (let [detections (:detections result)
        header "model_id,model_name,confidence,confidence_level,evidence,file_path,timestamp,has_lollapalooza"]
    (str
     header "
"
     (str/join "
"
               (map (fn [d]
                      (let [level (get-confidence-level (:confidence d))]
                        (str/join ","
                                  [(or (:model-id d) "")
                                   (str """ (or (:model-name d) "") """)
                                   (format "%.4f" (:confidence d))
                                   (:label level)
                                   (str """ (str/replace (or (:evidence d) "") """ """") """)
                                   (str """ (or (:file-path result) "") """)
                                   (:timestamp result (System/currentTimeMillis))
                                   (boolean (:lollapalooza result))])))
                    detections)))))

;; =============================================================================
;; PLAIN TEXT FORMATTER
;; =============================================================================

(defn format-text
  "Format analysis result as plain text."
  [result]
  (let [detections (:detections result)
        lollapalooza (:lollapalooza result)
        separator (str/join "" (repeat 60 "="))]
    (str
     separator "
"
     "MENTAL MODEL ANALYSIS
"
     separator "

"
     "Analyzed: " (java.util.Date. (:timestamp result (System/currentTimeMillis))) "
"
     (when (:file-path result)
       (str "Source: " (:file-path result) "
"))
     "
"
     (when lollapalooza
       (str
        (str/join "" (repeat 60 "*")) "
"
        "*** LOLLAPALOOZA EFFECT DETECTED ***
"
        (str/join "" (repeat 60 "*")) "

"
        (:description lollapalooza) "
"
        "Models: " (str/join ", " (map :model-name (:models lollapalooza))) "
"
        "Combined Confidence: " (format "%.1f%%" (* 100 (:combined-confidence lollapalooza))) "

"))
     "DETECTED MODELS (" (count detections) ")
"
     (str/join "" (repeat 40 "-")) "

"
     (str/join "

"
               (map-indexed (fn [i d]
                              (let [level (get-confidence-level (:confidence d))]
                                (str (inc i) ". " (:model-name d) "
"
                                     "   Confidence: " (format "%.1f%%" (* 100 (:confidence d)))
                                     " (" (:label level) ")
"
                                     "   Evidence: " (or (:evidence d) "N/A"))))
                            (sort-by :confidence > detections)))
     "

" separator)))

;; =============================================================================
;; UNIFIED FORMATTER
;; =============================================================================

(defn format-result
  "Format analysis result in specified format."
  [result format & opts]
  (log/debug "Formatting result" {:format format})
  (case format
    :json (apply format-json result opts)
    :html (format-html result)
    :markdown (format-markdown result)
    :md (format-markdown result)
    :csv (format-csv result)
    :text (format-text result)
    :txt (format-text result)
    (format-json result)))

;; =============================================================================
;; BATCH FORMATTING
;; =============================================================================

(defn format-results
  "Format multiple analysis results."
  [results format]
  (case format
    :json (json/write-str {:results (map #(json/read-str (format-json % :pretty false) :key-fn keyword) results)
                           :count (count results)
                           :timestamp (System/currentTimeMillis)}
                          :indent true)
    :csv (str (first (str/split-lines (format-csv (first results)))) "
"
              (str/join "
"
                        (mapcat (fn [r]
                                  (rest (str/split-lines (format-csv r))))
                                results)))
    (str/join "

" (map #(format-result % format) results))))
