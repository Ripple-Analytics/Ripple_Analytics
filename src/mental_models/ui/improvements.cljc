(ns mental-models.ui.improvements
  "Electric Clojure UI Components for Improvements Log"
  (:require
   [clojure.string :as str]
   [mental-models.history.improvements-log :as history]
   #?(:clj [hyperfiddle.electric :as e])))

;; Git-style improvements log UI with timeline view
;; Provides: ImprovementsLogFull, ModelImprovements, ImprovementsWidget

(defn format-relative-time [ts]
  #?(:clj
     (let [now (System/currentTimeMillis)
           diff (- now ts)
           minutes (/ diff 60000)
           hours (/ minutes 60)
           days (/ hours 24)]
       (cond
         (< minutes 1) "just now"
         (< minutes 60) (str (int minutes) " min ago")
         (< hours 24) (str (int hours) " hours ago")
         :else (history/format-timestamp ts)))
     :cljs ""))

#?(:clj
   (e/defn ImprovementsLogFull []
     (e/server
      (let [log (e/watch history/improvements-log)
            commits (take 30 (sort-by :timestamp > (:commits log)))]
        (e/client
         [:div.improvements-log
          [:h2 "Improvements Log"]
          [:div.stats
           [:span (str (count (:commits log)) " commits")]
           [:span (str (count (:branches log)) " branches")]]
          [:div.timeline
           (e/for [commit commits]
             [:div.commit
              [:span.hash (:hash commit)]
              [:span.msg (:message commit)]
              [:span.author (:author commit)]
              [:span.time (format-relative-time (:timestamp commit))]])]])))))

#?(:clj
   (e/defn ModelImprovements [model-id]
     (e/server
      (let [history (history/get-model-history model-id :limit 20)]
        (e/client
         [:div.model-history
          [:h3 (str "History: " model-id)]
          (if (empty? history)
            [:p "No changes recorded"]
            [:ul
             (e/for [commit history]
               [:li [:span.hash (:hash commit)] " " (:message commit)])])])))))

#?(:clj
   (e/defn ImprovementsWidget []
     (e/server
      (let [log (e/watch history/improvements-log)
            recent (take 5 (sort-by :timestamp > (:commits log)))]
        (e/client
         [:div.widget
          [:h4 "Recent Changes"]
          (e/for [commit recent]
            [:div.item
             [:span.hash (subs (:hash commit) 0 7)]
             " "
             (:message commit)])])))))
