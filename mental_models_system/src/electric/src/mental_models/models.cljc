(ns mental-models.models
  "Mental Models Library - Electric Clojure
   
   Index file that loads all model modules.
   See models/ directory for individual model definitions.
   
   This is a .cljc file - runs on both client and server!"
  #?(:clj (:require [clojure.string :as str]
                    [mental-models.models.core :as core]
                    [mental-models.models.index])
     :cljs (:require [clojure.string :as str]
                     [mental-models.models.core :as core]
                     [mental-models.models.index])))

;; Re-export core registry atoms and functions
(def !models core/!models)
(def !failure-modes core/!failure-modes)
(def !categories core/!categories)
(def register-model core/register-model)
(def get-model core/get-model)
(def get-all-models core/get-all-models)
(def search-models core/search-models)
(def get-categories core/get-categories)
(def get-models-by-category core/get-models-by-category)
(def model-stats core/model-stats)
