(ns meridian.datomic-rtree.point-test
  (:require [meridian.datomic-rtree.point :refer :all]
            [clojure.test :refer :all]
            [clojure/walk :as walk]
            [meridian.datomic-rtree.test-utils :as utils]
            [meridian.datomic-rtree.bbox :as bbox]
            [meridian.datomic-rtree.bulk :as bulk]))

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
      (utils/install-rand-ents conn 20 utils/minimal-entry)
      (utils/bulk-load-ents conn 6 3 bulk/dyn-cost-partition))))

(comment
  (defn ent [x] (d/entity (db conn) x))
  (use '[datomic.api :only (q db) :as d])
  (->> (d/q '[:find ?e :where [?e :rtree/root]] db)
       ffirst (d/entity db))
  (d/q '[:find ?e :where [?e :node/entry]] (db conn))
  (d/q '[:find ?e :where [?e]] (db conn))
  (def _ (doall (->> (d/datoms (db conn) :eavt) (map #(d/entity (db conn) (:e %))) distinct (map #(-> % seq pprint)))))
  (->> (utils/find-tree (db conn)) :rtree/root :node/children first :node/children first :node/entry :bbox)
  
  (pprint
   (tree-seq
    :node/children
    #(into {} (map seq (:node/children %)))
    (:rtree/root (utils/find-tree (db conn)))))

  (defn force-map [x] (seq x))
  
  (pprint
   (walk/prewalk
    (fn [x]
      (try (println (seq x)) (catch Exception e (println x)))
      (cond
       (:node/entry x) (force-map x)
       (:node/children x) (force-map x)
       :else x))
    (:rtree/root (utils/find-tree (db conn)))))
  )
