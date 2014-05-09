(ns meridian.datomic-rtree.distance-search
  (:use [datomic.api :only (q db) :as d])
  (:require [clojure.java.io :as io]
            [meridian.datomic-rtree.rtree :as rtree]
            [meridian.datomic-rtree.bbox :as bbox]
            [meridian.datomic-rtree.hilbert :as hilbert]
            [meridian.datomic-rtree.bulk :as bulk]
            [meridian.datomic-rtree.shapes :as shapes]
            [meridian.datomic-rtree.test-utils :as test-utils]
            [meridian.clj-jts :as jts]))

(comment
  (defn run-test-query1 [db1]
    (d/q '[:find ?e :where [?e :node/entry] ] db1)
    )

  (defn run-test-query2 [db1]
    (d/q '[:find ?e :where
           [?e :node/entry]
           [?e :bbox/max-x ?ex]
           [?e :bbox/max-y ?ey]
           [(meridian.datomic-rtree.test-utils/calc-distance ?ex ?ey) ?d]
           [(< ?d 10)]] db1)
    ))

(defn create-bbox-for-circle [[center-x center-y] radius]
  (let [min-x (- center-x radius)
        min-y (- center-y radius)
        width (* radius 2)
        height width]
    (bbox/bbox min-x min-y width height)))

(defn by-distance [[cx cy] r]
  (let [center-point (jts/point [cx cy])]
    (fn [node]
      (let [entry (:node/entry node)
            [x y] (read-string (:bbox entry))]
        (< (.distance (jts/point [x y]) center-point))))))

(defn distance-search [[center-x center-y] distance db]
  (let [bbox (create-bbox-for-circle [center-x center-y] distance)
        results-inside-bbox (rtree/intersecting (:rtree/root (test-utils/find-tree db)) bbox)]
    (filter (by-distance [center-x center-y] distance) results-inside-bbox)
    ))

(defn print-result [db1 result]
  (->> result first (d/entity db1) seq clojure.pprint/pprint))

(defn print-all-results [db1 results]
  (map (partial print-result db1) results))


(comment
  (test-utils/timings 1000 6 3)
  (def conn (d/connect "datomic:mem://rtrees"))
  (def db1 (d/db conn))
  (d/q '[:find ?e :where [?e :node/entry]] db1)
  (->> (d/q '[:find ?e :where [?e :node/entry]] db1) ffirst (d/entity db1) (seq) (clojure.pprint/pprint))
  (clojure.pprint/pprint (seq (map #(d/entity db1 %) (first (d/q '[:find ?e :where [?e :node/entry] [?e :bbox/max-y 269.27033000145093 ]] db1)))))
  (clojure.pprint/pprint (distance-search [133 76] 2 db1))
  )
