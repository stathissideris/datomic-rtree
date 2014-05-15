(ns meridian.datomic-rtree.params-test
  (:require [clojure.test :refer :all]
            [datomic.api :refer (q db) :as d]
            [meridian.datomic-rtree
             [rtree :as rtree]
             [test-utils :as utils]
             [bbox :as bbox]
             [bulk :as bulk]
             [shapes :as shapes]
             [hilbert :as hilbert]
             [search :as search]]))

(defn time-code
  "Runs fun and returns a map with the result as :result and the time
  in millis that took to run the function as :time"
  [fun]
  (let [start (System/nanoTime)
        result (fun)
        end (System/nanoTime)]
    {:result result
     :time (/ (double (- end start)) 1000000.0)}))

(defn- insert-point [conn tree point]
  (d/transact
   conn
   (rtree/insert-entry-tx
    tree
    point
    :install-entry true)))

(defn run-searches
  "Runs the a distance search with the given params and returns the
  timings of the various stages."
  [& {:keys (data-size insert-size searches max-children min-children index-fn) :as params}]
  (let [conn (utils/create-and-connect-db
              "datomic:mem://rtrees"
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        database (db conn)
        make-point (partial shapes/point-entry index-fn)
        points (take data-size (repeatedly #(make-point (rand-int 1000) (rand-int 1000))))
        _ (utils/create-tree conn max-children min-children)
        initial-insert-time (:time (time-code (fn [] @(d/transact conn points))))
        _ (println "Initial insert took" initial-insert-time "millis")
        index-time (:time (time-code #(bulk/bulk-index-entities conn max-children min-children bulk/dyn-cost-partition)))
        _ (println "Indexing took" index-time "millis")
        tree (rtree/find (db conn))
        insert-time
        (:time (time-code
                #(dotimes [_ insert-size]
                   @(insert-point conn tree (make-point (rand-int 1000) (rand-int 1000))))))
        _ (println "Insert took" insert-time "millis")
        search-time
        (:time (time-code
                #(dotimes [_ searches]
                   (search/distance-search [500 500] 500 database)
                   (print "."))))]
    (println)
    (let [times
          {:initial-insert-t initial-insert-time
           :index-t index-time
           :insert-t insert-time
           :search-t search-time}]
      (clojure.pprint/pprint times)
      [(dissoc params :index-fn) times])))

(defn seed-data
  "Seeds the data"
  [& {:keys (data-size insert-size searches max-children min-children index-fn) :as params}]
  (let [conn (utils/create-and-connect-db
              "datomic:mem://rtrees"
              "resources/datomic/schema.edn"
              "resources/datomic/geojsonschema.edn")
        database (db conn)
        make-point (partial shapes/point-entry index-fn)
        points (take data-size (repeatedly #(make-point (rand-int 1000) (rand-int 1000))))
        _ (utils/create-tree conn max-children min-children)
        initial-insert-time (:time (time-code (fn [] @(d/transact conn points))))
        _ (println "Initial insert took" initial-insert-time "millis")
        index-time (:time (time-code #(bulk/bulk-index-entities conn max-children min-children bulk/dyn-cost-partition)))
        _ (println "Indexing took" index-time "millis")]
    #_(search/distance-search [500 500] 50 database)
    conn))

(defn explore-max-min [data-size insert-size searches]
  (let [params
        (for [min (range 2 6)
              max (concat (range 3 15 2) [40 80])
              :when (> (- max min) 1)]
          [max min])]
    (println "Testing parameters with size" data-size "and"
             searches "searches.\n" (count params) "combinations of parameters.")
    (doall
     (map (fn [combination [max-children min-children]]
            (println (format "Running param combination %d out of %d: %s"
                             (inc combination)
                             (count params)
                             (pr-str {:max max-children :min min-children})))
            (run-searches :data-size data-size
                          :insert-size insert-size
                          :searches searches
                          :max-children max-children
                          :min-children min-children
                          :index-fn (hilbert/index-fn 28 [0.0 1000.0])))
          (range (count params)) params))))

(defn round-numbers [m]
  (reduce-kv (fn [m k v] (assoc m k (int v))) {} m))

(comment
  (def res (explore-max-min 10000 0 1000))
  (def res (explore-max-min 10000 500 1000))
  (print-table (sort-by (juxt :min-children :max-children)
                        (map (fn [[a b]] (round-numbers (merge a b))) res))))
