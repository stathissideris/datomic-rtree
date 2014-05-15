(ns meridian.datomic-rtree.point-test
  (:require [meridian.datomic-rtree.point :refer :all]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [datomic.api :refer (q db) :as d]
            [dali.svg-translate :as dali]
            [meridian.datomic-rtree
             [rtree :as rtree]
             [test-utils :as utils]
             [bbox :as bbox]
             [bulk :as bulk]
             [shapes :as shapes]
             [hilbert :as hilbert]
             [search :as search]]
            [meridian.clj-jts :as jts]))

(defn connect-datomic []
  (utils/create-and-connect-db
   "datomic:mem://rtrees"
   "resources/datomic/schema.edn"
   "resources/datomic/geojsonschema.edn"))

(defn distance-search
  [[x y] distance database]
  (->> (search/distance-search [x y] distance database)
       (map (fn [x] ((juxt :x :y) (->> x :node/entry seq (into {})))))
       (into #{})))

(defn polygon-search
  [polygon database]
  (->> (search/polygon-search polygon database)
       (map (fn [x] ((juxt :x :y) (->> x :node/entry seq (into {})))))
       (into #{})))

(defn insert-points-and-index [conn points max-children min-children]
  (utils/create-tree conn max-children min-children)
  @(d/transact conn points)
  (bulk/bulk-index-entities conn max-children min-children bulk/dyn-cost-partition))

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
        point (partial shapes/point-entry hilbert-index-fn)
        point-count 30
        search-bbox (fn [bbox]
                      (->> (rtree/intersecting
                            (:rtree/root (utils/find-tree (db conn))) bbox)
                           (map (fn [x] ((juxt :x :y) (->> x :node/entry))))
                           (into #{})))
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
           #{[4.0 4.0] [1.0 1.0] [5.0 3.0] [1.0 3.0] [3.0 1.0]}
           (search-bbox (bbox/bbox 0 0 50 50))))
      (is (=
           #{[90.0 90.0] [200.0 100.0] [100.0 100.0] [110.0 110.0]
             [120.0 120.0] [220.0 90.0] [200.0 90.0] [220.0 105.0]}
           (search-bbox (bbox/bbox 80 80 230 150)))))))

(deftest test-distance-search
  (let [uri "datomic:mem://rtrees"
        conn (utils/create-and-connect-db
              uri
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        hilbert-index-fn (hilbert/index-fn 28 [0.0 600.0])
        point (partial shapes/point-entry hilbert-index-fn)
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
      (is (= #{[1.0 1.0] [1.0 3.0] [3.0 1.0]}
             (distance-search [2.0 2.0] 2.4 (db conn)))))))

(deftest test-polygon-search
  (let [uri "datomic:mem://rtrees"
        conn (utils/create-and-connect-db
              uri
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        hilbert-index-fn (hilbert/index-fn 28 [0.0 600.0])
        point (partial shapes/point-entry hilbert-index-fn)
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
      (is (= #{[4.0 4.0] [5.0 3.0]}
             (polygon-search [[[2 2] [2 100] [80 100] [100 2] [2 2]]] (db conn)))))))

(defn- svg-path->jts-polygon
  "Converts an SVG path specification into a JTS polygon
  specification (just the vector, doesn't call the constructor on
  them. This is not supposed to be a generic function, but it works on
  polyline paths generated using Inkscape."
  [svg-path]
  (let [points
        (->> (string/split svg-path #" ")
             (rest)
             (butlast)
             (map #(string/split % #","))
             (map (fn [[x y]] [(Double/parseDouble x) (Double/parseDouble y)]))
             (reductions (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)]))
             (into []))]
    (vector (conj points (first points)))))

(defn- insert-test-points [filename conn]
  (let [hilbert-index-fn (hilbert/index-fn 28 [0.0 1001.0])
        make-point (partial shapes/point-entry hilbert-index-fn)
        raw-points (-> filename io/resource slurp read-string)
        points (map (fn [[x y]] (make-point x y)) raw-points)]
    (insert-points-and-index conn points 6 3)))

(defn generate-visual-polygon-test []
  (let [dataset-filename "test-data/search-points.edn"
        conn (connect-datomic)
        save-result (fn [filename points] (spit filename (with-out-str (pprint points))))
        raw-points (-> dataset-filename io/resource slurp read-string)]
    (insert-test-points dataset-filename conn)
    (let [distance-matches (distance-search [369.2355 316.3675000000001] 107.9805 (db conn))

          star-path "m 735,313 18,-66 17,-59 33,11 5,59 -22,66 117,-14 9,23 -37,52 -105,11 -54,109 -49,-61 44,-95 -27,-30 -21,0 -28,-7 1,-93 17,-28 13,57 54,41 z"
          path1-matches (polygon-search (svg-path->jts-polygon star-path) (db conn))

          pi-path "m 301,829 29,-193 239,-3 17,283 -85,-12 1,-200 -106,-4 -11,144 z"
          path2-matches (polygon-search (svg-path->jts-polygon pi-path) (db conn))
          
          svg [:page {:height 1000 :width 1000 :stroke {:paint :black :width 1} :fill :none}

               ;;visualize queries
               [:circle {:stroke {:paint :green :width 2}} [369.2355 316.3675000000001] 107.9805]
               [:path {:stroke {:paint :red :width 2} :d star-path}]
               [:path {:stroke {:paint :pink :width 2} :d pi-path}]

               ;;visualize database contents
               [:g {:id :all-data, :stroke :none, :fill "blue"}
                (map (fn [[x y]] [:circle [x y] 2]) raw-points)]

               ;;visualize matches
               [:g {:id :distance-matches, :stroke {:paint :green, :width 1.5} :fill :none}
                (map (fn [[x y]] [:circle [x y] 5]) distance-matches)]
               [:g {:id :path1-matches, :stroke {:paint :red, :width 1.5} :fill :none}
                (map (fn [[x y]] [:circle [x y] 5]) path1-matches)]
               [:g {:id :path2-matches, :stroke {:paint :pink, :width 1.5} :fill :none}
                (map (fn [[x y]] [:circle [x y] 5]) path2-matches)]]]

      (save-result "resources/test-data/distance-expected.edn" distance-matches)
      (save-result "resources/test-data/path1-expected.edn" path1-matches)
      (save-result "resources/test-data/path2-expected.edn" path2-matches)
      (-> svg dali/dali->hiccup (dali/spit-svg "/var/www/points-query.svg")))))

(deftest test-advanced-polygon-search
  (let [conn (connect-datomic)
        hilbert-index-fn (hilbert/index-fn 28 [0.0 1001.0])
        make-point (partial shapes/point-entry hilbert-index-fn)]
    (println "Creating tree and inserting points in bulk...")
    (insert-test-points "test-data/search-points.edn" conn)
    (let [tree (rtree/find (db conn))]
      (println "Inserting one more point within circle...")
      @(d/transact conn (rtree/insert-entry-tx tree (make-point 369.2355 316.3675000000001) :install-entry true))
      (println "Testing searches...")
      (testing "distance (circle) search"
        (is (= (conj (-> "test-data/distance-expected.edn" io/resource slurp read-string)
                     [369.2355 316.3675000000001])
               (distance-search [369.2355 316.3675000000001] 107.9805 (db conn)))))
      (testing "star-like polygon search"
        (println "Inserting one more point within star...")
        @(d/transact conn (rtree/insert-entry-tx tree (make-point 700 428) :install-entry true))
        (is (= (conj (-> "test-data/path1-expected.edn" io/resource slurp read-string) [700.0 428.0])
               (polygon-search (svg-path->jts-polygon
                                "m 735,313 18,-66 17,-59 33,11 5,59 -22,66 117,-14 9,23 -37,52 -105,11 -54,109 -49,-61 44,-95 -27,-30 -21,0 -28,-7 1,-93 17,-28 13,57 54,41 z")
                               (db conn))))))))

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
