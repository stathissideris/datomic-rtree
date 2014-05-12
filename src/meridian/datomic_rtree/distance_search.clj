(ns meridian.datomic-rtree.distance-search
  (:use [datomic.api :only (db) :as d])
  (:require [clojure.java.io :as io]
            [meridian.datomic-rtree.rtree :as rtree]
            [meridian.datomic-rtree.bbox :as bbox]
            [meridian.clj-jts :as jts]))

(defn- create-bbox-for-circle [[center-x center-y] radius]
  (let [min-x (- center-x radius)
        min-y (- center-y radius)
        width (* radius 2)
        height width]
    (bbox/bbox min-x min-y width height)))

(defn- by-distance [[cx cy] r]
  (let [center-point (jts/point [cx cy])]
    (fn [node]
      (let [entry (:node/entry node)
            [x y] (read-string (:bbox entry))]
        (<= (.distance (jts/point [x y]) center-point) r)))))

(defn distance-search [[center-x center-y] distance db]
  (let [bbox (create-bbox-for-circle [center-x center-y] distance)
        results-inside-bbox (rtree/intersecting (:rtree/root (rtree/find db)) bbox)]
    (filter (by-distance [center-x center-y] distance) results-inside-bbox)
    ))
