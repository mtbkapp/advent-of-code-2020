(ns advent-of-code-2020.utils
  (:require [clojure.java.io :as io]))


(defn read-lines
  ([file]
   (read-lines (map identity) file))
  ([xform file]
   (with-open [rdr (io/reader (io/resource file))]
     (into [] xform (line-seq rdr)))))
