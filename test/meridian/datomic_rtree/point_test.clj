(ns meridian.datomic-rtree.point-test
  (:require [meridian.datomic-rtree.point :refer :all]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [datomic.api :refer (q db) :as d]
            [dali.svg-translate :as dali]
            [meridian.datomic-rtree
             [rtree :as rtree]
             [test-utils :as utils]
             [bbox :as bbox]
             [bulk :as bulk]
             [shapes :as shapes]
             [hilbert :as hilbert]
             [distance-search :as dist-search]
             [polygon-search :as poly-search]]
            [meridian.clj-jts :as jts]))

(defn connect-datomic []
  (utils/create-and-connect-db
   "datomic:mem://rtrees"
   "resources/datomic/schema.edn"
   "resources/datomic/geojsonschema.edn"))

(defn distance-search
  [[x y] distance database]
  (->> (dist-search/distance-search [x y] distance database)
       (map (fn [x] (->> x :node/entry seq (into {}))))
       (into #{})))

(defn polygon-search
  [polygon database]
  (->> (poly-search/polygon-search polygon database)
       (map (fn [x] (->> x :node/entry seq (into {}))))
       (into #{})))

(defn insert-points-and-index [conn points max-children min-children]
  (utils/create-tree conn max-children min-children)
  @(d/transact conn points)
  (utils/bulk-load-ents conn max-children min-children bulk/dyn-cost-partition))

(deftest point-contained-test
  (are [p bbox] (true? (contained? p bbox))
       (point 5 20) (bbox/bbox 0 0 30 30)
       (point 29 29) (bbox/bbox 0 0 30 30))
  (are [p bbox] (false? (contained? p bbox))
       (point -10 15) (bbox/bbox 0 0 30 30)
       (point 15 40) (bbox/bbox 0 0 30 30)))

(deftest insert-point-to-tree
  (let [uri "datomic:mem://rtrees"
        conn (utils/create-and-connect-db
              uri
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        hilbert-index-fn (hilbert/index-fn 28 [0.0 600.0])
        point (partial utils/point-entry hilbert-index-fn)
        point-count 30
        search-bbox (fn [bbox]
                      (->> (rtree/intersecting
                            (:rtree/root (utils/find-tree (db conn))) bbox)
                           (map (fn [x] (->> x :node/entry seq (into {}))))
                           (into #{})))
        _ (def search-bbox search-bbox);;TODO clean up
        points [(point 1 1)
                (point 5 3)
                (point 1 3)
                (point 3 1)
                (point 4 4)

                (point 100 100)
                (point 110 110)
                (point 90 90)
                (point 120 120)
                (point 200 100)
                (point 220 105)
                (point 220 90)
                (point 200 90)

                (point 100 350)
                (point 110 360)
                (point 90 330)
                (point 100 300)
                (point 120 320)
                (point 100 360)

                (point 550 500)]]
    (insert-points-and-index conn points 6 3)
    (testing "point insertion to r-tree"
      (is (=
           #{{:bbox "[5.0 3.0 5.0 3.0]\n", :type :Point}
             {:bbox "[1.0 1.0 1.0 1.0]\n", :type :Point}
             {:bbox "[1.0 3.0 1.0 3.0]\n", :type :Point}
             {:bbox "[4.0 4.0 4.0 4.0]\n", :type :Point}
             {:bbox "[3.0 1.0 3.0 1.0]\n", :type :Point}}
           (search-bbox (bbox/bbox 0 0 50 50))))
      (is (=
           #{{:bbox "[110.0 110.0 110.0 110.0]\n", :type :Point}
             {:bbox "[100.0 100.0 100.0 100.0]\n", :type :Point}
             {:bbox "[200.0 100.0 200.0 100.0]\n", :type :Point}
             {:bbox "[220.0 105.0 220.0 105.0]\n", :type :Point}
             {:bbox "[90.0 90.0 90.0 90.0]\n", :type :Point}
             {:bbox "[220.0 90.0 220.0 90.0]\n", :type :Point}
             {:bbox "[200.0 90.0 200.0 90.0]\n", :type :Point}
             {:bbox "[120.0 120.0 120.0 120.0]\n", :type :Point}}
           (search-bbox (bbox/bbox 80 80 230 150)))))))

(deftest test-distance-search
  (let [uri "datomic:mem://rtrees"
        conn (utils/create-and-connect-db
              uri
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        hilbert-index-fn (hilbert/index-fn 28 [0.0 600.0])
        point (partial utils/point-entry hilbert-index-fn)
        points [(point 1 1)
                (point 5 3)
                (point 1 3)
                (point 3 1)
                (point 4 4)

                (point 100 100)
                (point 110 110)
                (point 90 90)
                (point 120 120)
                (point 200 100)
                (point 220 105)
                (point 220 90)
                (point 200 90)

                (point 100 350)
                (point 110 360)
                (point 90 330)
                (point 100 300)
                (point 120 320)
                (point 100 360)

                (point 550 500)]]
    (insert-points-and-index conn points 6 3)
    (testing "Distance search"
      (let [res (distance-search [2.0 2.0] 2.4 (db conn))]
        (is (=
             #{{:bbox "[1.0 1.0 1.0 1.0]\n", :type :Point}
               {:bbox "[1.0 3.0 1.0 3.0]\n", :type :Point}
               {:bbox "[3.0 1.0 3.0 1.0]\n", :type :Point}}
             res))))))

(deftest test-polygon-search
  (let [uri "datomic:mem://rtrees"
        conn (utils/create-and-connect-db
              uri
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        hilbert-index-fn (hilbert/index-fn 28 [0.0 600.0])
        point (partial utils/point-entry hilbert-index-fn)
        points [(point 1 1)
                (point 5 3)
                (point 1 3)
                (point 3 1)
                (point 4 4)

                (point 100 100)
                (point 110 110)
                (point 90 90)
                (point 120 120)
                (point 200 100)
                (point 220 105)
                (point 220 90)
                (point 200 90)

                (point 100 350)
                (point 110 360)
                (point 90 330)
                (point 100 300)
                (point 120 320)
                (point 100 360)

                (point 550 500)]]
    (insert-points-and-index conn points 6 3)
    (testing "polygon search"
      (is (=
           #{{:bbox "[4.0 4.0 4.0 4.0]\n", :type :Point}
             {:bbox "[5.0 3.0 5.0 3.0]\n", :type :Point}
             }
           (polygon-search (jts/polygon [[[2 2] [2 100] [80 100] [100 2] [2 2]]]) (db conn)))))))

(defn test-advanced-polygon-search []
  (let [conn (connect-datomic)
        hilbert-index-fn (hilbert/index-fn 28 [0.0 1001.0])
        make-point (partial utils/point-entry hilbert-index-fn)
        raw-points (-> "test-data/search-points.edn" io/resource slurp read-string)
        points (map (fn [[x y]] (make-point x y)) raw-points)]
    (insert-points-and-index conn points 6 3)
    (let [matches (map
                   #(-> % :bbox read-string)
                   (distance-search [369.2355 616.3675000000001] 107.9805 (db conn)))
          svg [:page {:height 1000 :width 1000 :stroke {:paint :black :width 1} :fill :none}
               [:circle [369.2355 616.3675000000001] 107.9805]
               [:g {:id :all-data, :stroke :black, :fill "lightgray"}
                (map (fn [[x y]] [:circle [x y] 2]) raw-points)]
               [:g {:id :distance-matches, :stroke :green, :fill "springgreen"}
                (map (fn [[x y]] [:circle [x y] 3]) matches)]]]
      (-> svg dali/dali->hiccup (dali/spit-svg "/var/www/points-query.svg")))))

#_(test-advanced-polygon-search)

(def distance-query [:circle [369.2355 616.3675000000001] 107.9805])

(def pi-query "m 301,829 29,-193 239,-3 17,283 -85,-12 1,-200 -106,-4 -11,144 z")

(def star-query "m 735,313 18,-66 17,-59 33,11 5,59 -22,66 117,-14 9,23 -37,52 -105,11 -54,109 -49,-61 44,-95 -27,-30 -21,0 -28,-7 1,-93 17,-28 13,57 54,41 z")


(comment
  (use '[datomic.api :only (q db) :as d])
  (defn ent [x] (d/entity (db conn) x))
  (->> (d/q '[:find ?e :where [?e :rtree/root]] db)
       ffirst (d/entity db))
  (d/q '[:find ?e :where [?e :node/entry]] (db conn))
  (d/q '[:find ?e :where [?e]] (db conn))
  (def _ (doall (->> (d/datoms (db conn) :eavt) (map #(d/entity (db conn) (:e %))) distinct (map #(-> % seq pprint)))))
  (->> (utils/find-tree (db conn)) :rtree/root :node/children first :node/children first :node/entry :bbox)
  (rtree/containing (:rtree/root (utils/find-tree (db conn))) (bbox/bbox 0 0 50 50)))
