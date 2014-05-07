(ns user
  (:require
            [clojure.java.javadoc :refer (javadoc)]
            [clojure.pprint :refer (pprint print-table pp)]
            [clojure.reflect :refer (reflect)]
            [clojure.repl :refer (apropos dir doc find-doc pst source)]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [clojure.tools.trace :refer (trace deftrace trace-forms trace-ns trace-vars)]))

(defn reset [] (refresh))

(defn run-all-my-tests []
  (reset)
  (test/run-all-tests #"meridian.datomic-rtree.*-test$"))
