(ns breakout.utils)

(defn concatV [v1 v2]
  "concatenates two vectors and returns a vector"
  (into [] (concat v1 v2)))

(defn gen2DCoords [x y]
  "produces a vector of paired points (each is a vector) from
   [[0 0] ... [x-1 y-1]]"
  (reduce 
    (fn [acc elem]
      (concatV acc (map (fn [e] [elem e]) (range x))))
    []
    (range y))) 

(defn inv [n] (* -1 n))

(defn outside? [num lower upper]
  (or (< num lower)  (> num upper)))