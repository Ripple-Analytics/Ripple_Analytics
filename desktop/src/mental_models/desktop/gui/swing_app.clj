(ns mental-models.desktop.gui.swing-app
  "Swing Application - Index File
   
   This namespace loads all swing GUI modules.
   See swing/ directory for individual component implementations."
  (:require [mental-models.desktop.gui.swing.config :as config]
            [mental-models.desktop.gui.swing.state :as state]
            [mental-models.desktop.gui.swing.theme :as theme]
            [mental-models.desktop.gui.swing.database :as database]
            [mental-models.desktop.gui.swing.http-client :as http-client]
            [mental-models.desktop.gui.swing.connection-checks :as connection-checks]
            [mental-models.desktop.gui.swing.lm-studio :as lm-studio]
            [mental-models.desktop.gui.swing.file-scanning :as file-scanning]
            [mental-models.desktop.gui.swing.batch-queue :as batch-queue]
            [mental-models.desktop.gui.swing.cli-batch :as cli-batch]
            [mental-models.desktop.gui.swing.watch-mode :as watch-mode]
            [mental-models.desktop.gui.swing.version-utils :as version-utils]
            [mental-models.desktop.gui.swing.error-reporting :as error-reporting]
            [mental-models.desktop.gui.swing.sidebar :as sidebar]
            [mental-models.desktop.gui.swing.main-frame :as main-frame]
            [mental-models.desktop.gui.swing.main-entry :as main-entry]
            [mental-models.desktop.gui.swing.feature-request-dialog :as feature-request-dialog]))

;; Re-export main entry point
(def -main main-entry/-main)
(def create-main-frame main-frame/create-main-frame)
(def init-database! database/init-database!)
(def get-app-state state/get-app-state)
(def update-app-state! state/update-app-state!)
