#!/usr/bin/env clojure
;; Generate Release Manifest for Delta Updates
;; 
;; Usage: clojure scripts/generate-manifest.clj <version> <release-dir> <output-file>
;; Example: clojure scripts/generate-manifest.clj v2.1.0 Mental_Models_build manifest.edn

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(import '[java.io File FileInputStream]
        '[java.security MessageDigest])

(defn file-hash [^File file]
  "Calculate SHA-256 hash of a file"
  (let [digest (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 8192)]
    (with-open [fis (FileInputStream. file)]
      (loop []
        (let [n (.read fis buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn build-manifest [version base-dir base-url]
  "Build a manifest for a release directory"
  (let [base-path (.toPath (io/file base-dir))
        files (->> (file-seq (io/file base-dir))
                   (filter #(.isFile %))
                   (filter #(not (str/includes? (.getPath %) ".git")))
                   (filter #(not (str/ends-with? (.getName %) ".class")))
                   (map (fn [f]
                          (let [rel-path (str (.relativize base-path (.toPath f)))]
                            {:path rel-path
                             :hash (file-hash f)
                             :size (.length f)
                             :modified (.lastModified f)})))
                   (sort-by :path)
                   (into []))]
    {:version version
     :timestamp (System/currentTimeMillis)
     :base-url base-url
     :file-count (count files)
     :total-size (reduce + (map :size files))
     :files files}))

(defn -main [& args]
  (if (< (count args) 3)
    (do
      (println "Usage: generate-manifest.clj <version> <release-dir> <output-file> [base-url]")
      (println "Example: generate-manifest.clj v2.1.0 Mental_Models_build manifest.edn")
      (System/exit 1))
    (let [[version release-dir output-file base-url] args
          base-url (or base-url "https://raw.githubusercontent.com/Ripple-Analytics/Ripple_Analytics/master/Mental_Models_build")]
      (println (str "Generating manifest for " version "..."))
      (println (str "  Release dir: " release-dir))
      (println (str "  Base URL: " base-url))
      
      (let [manifest (build-manifest version release-dir base-url)]
        (spit output-file (pr-str manifest))
        (println (str "  Files: " (:file-count manifest)))
        (println (str "  Total size: " (format "%.2f MB" (/ (:total-size manifest) 1048576.0))))
        (println (str "  Output: " output-file))
        (println "Done!")))))

(apply -main *command-line-args*)
