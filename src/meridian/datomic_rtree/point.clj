(ns meridian.datomic-rtree.point)

(defn point [x y]
  {:point/x x :point/y y})

(defn contained? [point bbox]
  (let [{max-x :bbox/max-x min-x :bbox/min-x
         max-y :bbox/max-y min-y :bbox/min-y} bbox
         {x :point/x y :point/y} point]
    (and (<= min-x x max-x)
         (<= min-y y max-y))))
