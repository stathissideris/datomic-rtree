(ns meridian.datomic-rtree.point-test
  (:require [meridian.datomic-rtree.point :refer :all]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [datomic.api :refer (q db) :as d]
            [meridian.datomic-rtree
             [rtree :as rtree]
             [test-utils :as utils]
             [bbox :as bbox]
             [bulk :as bulk]
             [shapes :as shapes]
             [hilbert :as hilbert]
             [distance-search :as dist-search]]))

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
        max 6
        min 3
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
    (do
      (def conn conn);;TODO clean up
      (utils/create-tree conn max min)
      ;;(utils/install-rand-ents conn point-count utils/minimal-entry)
      @(d/transact conn points)
      (utils/bulk-load-ents conn max min bulk/dyn-cost-partition))
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
        max 6
        min 3
        point-count 30

        points [(point 1 1)
                (point 5 3)
                (point 1 3)
                (point 3 1)
                (point 4 4)]]
    (do
      (def conn conn);;TODO clean up
      (utils/create-tree conn max min)
      ;;(utils/install-rand-ents conn point-count utils/minimal-entry)
      @(d/transact conn points)
      (utils/bulk-load-ents conn max min bulk/dyn-cost-partition))
    (testing "point insertion to r-tree"
      (is (=
           #{{:bbox "[5.0 3.0 5.0 3.0]\n", :type :Point}
             {:bbox "[1.0 1.0 1.0 1.0]\n", :type :Point}
             {:bbox "[1.0 3.0 1.0 3.0]\n", :type :Point}
             {:bbox "[4.0 4.0 4.0 4.0]\n", :type :Point}
             {:bbox "[3.0 1.0 3.0 1.0]\n", :type :Point}}
           (dist-search/distance-search [2.0 2.0] 2 (db conn))))
      )))


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
