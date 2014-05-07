(ns meridian.datomic-rtree.point-test
  (:require [meridian.datomic-rtree.point :refer :all]
            [clojure.test :refer :all]
            [meridian.datomic-rtree.test-utils :as utils]
            [meridian.datomic-rtree.bbox :as bbox]))

(deftest point-contained-test
  (are [p bbox] (true? (contained? p bbox))
       (point 5 20) (bbox/bbox 0 0 30 30)
       (point 29 29) (bbox/bbox 0 0 30 30))
  (are [p bbox] (false? (contained? p bbox))
       (point -10 15) (bbox/bbox 0 0 30 30)
       (point 15 40) (bbox/bbox 0 0 30 30)))

(deftest insert-point-to-tree
  (testing "point insertion to r-tree"
    (let [uri "datomic:mem://rtrees"
          conn (utils/create-and-connect-db
                uri
                "resources/datomic/schema.edn"
                "resources/datomic/geojsonschema.edn")]
      (def conn conn)
      (utils/create-tree conn 6 3)
      (utils/install-rand-ents conn 5 utils/minimal-entry))))

