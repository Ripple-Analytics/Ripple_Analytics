(ns mental-models.main.section-1-part2
  "Main Module - Section 1 Part2"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio"
       :fallback (data/classify-by-mental-models text)
       :llm-powered false})))

