(ns breakout.collisions)

(defn colliding? [e1 e2]
  "performs rectangle collision between e1 and e2
   :x - x position of corner
   :y - y position of corner
   :w - width of entity
   :h - height of entity"
  (let [x1 (:x e1)
        x2 (:x e2)
        y1 (:y e1)
        y2 (:y e2)
        w1 (:w e1)
        w2 (:w e2)
        h1 (:h e1)
        h2 (:h e2)
        
        cond1 (< x2 (+ x1 w1))
        cond2 (< x1 (+ x2 w2))
        cond3 (< y2 (+ y1 h1))
        cond4 (< y1 (+ y2 h2))]
    (and cond1 cond2 cond3 cond4)))


(defn processCollisions [collider listOfObjects collFcn]
  "Runs a collision check between collider entity and a list
   of entities called listOfObjects. If there is a collision,
   it will call collFcn on the entity in listOfObjects. It will return
   [collision? newListOfObjects] where collision? is whether any collision
   occured, and newListOfObjects is the list with collFcn applied to collied
   objects.

   collider - entity to run against all other entities
   listOfObjects - list of entities to be tested against
   collFcn - function of form (fn [entity] ...) which will be called on 
      an entity in listOfObjects in event of a collision
   
   returns [didCollisionOccur? newListOfObjects]"
  (reduce 
    ; accumulator is whether a collision has occured yet, and newListOfObjects
    (fn [[accColl? accList] entity]
      (let [coll? (colliding? collider entity)
            entity (if coll? (collFcn entity) entity)]
        [(or coll? accColl?) (cons entity accList)]))
    [false []]
    (reverse listOfObjects)))