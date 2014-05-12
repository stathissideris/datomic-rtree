(ns meridian.datomic-rtree.polygon-search
  (:use [datomic.api :only (q db) :as d])
  (:require [clojure.java.io :as io]
            [meridian.datomic-rtree.rtree :as rtree]
            [meridian.datomic-rtree.bbox :as bbox]
            [meridian.datomic-rtree.hilbert :as hilbert]
            [meridian.datomic-rtree.bulk :as bulk]
            [meridian.datomic-rtree.shapes :as shapes]
            [meridian.datomic-rtree.test-utils :as test-utils]
            [meridian.clj-jts :as jts])
  )

(defn create-bbox-for-polygon [p]
  (let [envelope (.getEnvelopeInternal p)
        min-x (.getMinX envelope)
        max-x (.getMaxX envelope)
        min-y (.getMinY envelope)
        max-y (.getMaxY envelope)
        width (Math/abs (- max-x min-x))
        height (Math/abs (- max-y min-y))
        ]
    (bbox/bbox min-x min-y width height)
    ))

(defn by-polygon [polygon]
  (fn [node]
    (let [entry (:node/entry node)
          [x y] (read-string (:bbox entry))]
      (.contains polygon (jts/point [x y]))))
  )

(defn polygon-search [polygon db]
  (let [bbox (create-bbox-for-polygon polygon)
        results-inside-bbox (rtree/intersecting (:rtree/root (test-utils/find-tree db)) bbox)]
    (filter (by-polygon polygon) results-inside-bbox)
    ))
