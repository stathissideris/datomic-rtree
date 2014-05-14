(ns meridian.datomic-rtree.search
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
      (<= (.distance (jts/point ((juxt :x :y) (:node/entry node))) center-point) r))))

(defn distance-search [[center-x center-y] distance db]
  (let [bbox (create-bbox-for-circle [center-x center-y] distance)
        results-inside-bbox (rtree/intersecting (:rtree/root (rtree/find db)) bbox)]
    (filter (by-distance [center-x center-y] distance) results-inside-bbox)))

(defn- create-bbox-for-polygon [p]
  (let [envelope (.getEnvelopeInternal p)
        min-x (.getMinX envelope)
        max-x (.getMaxX envelope)
        min-y (.getMinY envelope)
        max-y (.getMaxY envelope)
        width (Math/abs (- max-x min-x))
        height (Math/abs (- max-y min-y))]
    (bbox/bbox min-x min-y width height)))

(defn- by-polygon [polygon]
  (fn [node]
    (.contains polygon (jts/point ((juxt :x :y) (:node/entry node))))))

(defn polygon-search [polygon db]
  (let [bbox (create-bbox-for-polygon polygon)
        results-inside-bbox (rtree/intersecting (:rtree/root (rtree/find db)) bbox)]
    (filter (by-polygon polygon) results-inside-bbox)))

(defn polygon-search-points
  "Search for points inside the declared polygon.
The polygon is a vector of point vectors should be passed in to create a polygon (it must be closed)."
  [poly-points db]
  (polygon-search (jts/polygon poly-points) db))
