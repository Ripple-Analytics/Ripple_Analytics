(ns mental-models.desktop.watcher
  "File system watcher for automatic processing"
  (:require [hawk.core :as hawk]
            [taoensso.timbre :as log]))

(defonce watchers (atom {}))

(defn start-watching [folder-path callback]
  (log/info "Starting file watcher for:" folder-path)
  (let [watcher (hawk/watch! [{:paths [folder-path]
                               :filter hawk/file?
                               :handler (fn [ctx event]
                                          (callback event)
                                          ctx)}])]
    (swap! watchers assoc folder-path watcher)
    watcher))

(defn stop-watching [folder-path]
  (when-let [watcher (get @watchers folder-path)]
    (log/info "Stopping file watcher for:" folder-path)
    (hawk/stop! watcher)
    (swap! watchers dissoc folder-path)))

(defn stop-all-watchers []
  (doseq [[path watcher] @watchers]
    (log/info "Stopping watcher:" path)
    (hawk/stop! watcher))
  (reset! watchers {}))
