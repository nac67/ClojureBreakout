(ns breakout.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

(def ball_path "ball.png")
(def brick_path "brick.png")
(def paddle_path "paddle.png")

; concatenates two vectors and returns a vector
(defn concatV [v1 v2]
  (into [] (concat v1 v2)))

; given x dimension and y dimension, outputs list of tuples in form
; [[0 0] [0 1] [0 2] [0 3] ... [1 0] [1 1] [1 2]...[x-1 y-1]]
(defn gen2DCoords [x y]
  (reduce 
    (fn [acc elem]
      (concatV acc (map (fn [e] [elem e]) (range x))))
    []
    (range y))) 

(defn addBricks [entities]
  (concatV entities 
     (map (fn [[x y]]
        (assoc (texture brick_path)
               :game-type "brick"
               :x (+ (* x 70) 200)
               :y (- 530 (* y 30))
               :w 60
               :h 20))
        (gen2DCoords 5 6))))

(defn initGame []
  (-> [(assoc (texture ball_path) :game-type "ball" :x 400 :y 300 :vx 5 :vy 5 :w 20 :h 20)
       (assoc (texture paddle_path) :game-type "paddle" :x 400 :y 30 :w 100 :h 20)]
      (addBricks)))

(defn outside? [num lower upper]
  (or (< num lower)  (> num upper)))
  
(defn inv [n] (* -1 n))

(defn move [entity x y]
  (let [newX (+ (:x entity) x)
        newY (+ (:y entity) y)]
    (assoc entity :x newX :y newY)))

(defn bounce [entity]
  (let [newVx (if (outside? (:x entity) 0 800) (inv (:vx entity)) (:vx entity))
        newVy (if (> (:y entity) 600) (inv (:vy entity)) (:vy entity))]
    (assoc entity :vx newVx :vy newVy)))

(defn fallOffScreen [ball]
  (if (< (:y ball) -100)
             (assoc ball :x 400 :y 300)
             ball))

(defn updateBall [ball]
  (-> ball (move (:vx ball) (:vy ball)) 
           (bounce)
           (fallOffScreen)))

(defn colliding? [e1 e2]
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
  (let [listOfObjects 
          (map (fn [obj] 
            (if (colliding? collider obj) 
              (collFcn obj)
                obj))
            listOfObjects)
        collisionOccured? (some (fn [obj] (colliding? collider obj)) listOfObjects)]
    
    [collisionOccured? listOfObjects]))
    
(defn processBreakoutCollisions [ball paddle bricks]
    (let [[collisions? [paddle & bricks]] 
           (processCollisions 
             ball 
             (cons paddle bricks) ; collide ball against paddle or bricks
             (fn [hitObject]
                (cond
                   (= (:game-type hitObject) "brick")
                      (assoc hitObject :destroyed true) ; mark brick as destroyed
                   :default
                       hitObject))) ; if object not a brick, leave it alone
           ball (if collisions? (assoc ball :vy (inv (:vy ball))) ball)
           bricks (filter (fn [brick] (not (:destroyed brick))) bricks)]
      
      [ball bricks]))
    

(defn update [screen [ball paddle & bricks :as entities]]
  (let [newBall (updateBall ball)
        newPaddle (assoc paddle :x (- (:mouse-x screen) 40))
        [newBall2 newBricks] (processBreakoutCollisions newBall paddle bricks)]
    (concatV [newBall2 newPaddle] newBricks)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (initGame))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen (update screen entities)))
  
  :on-mouse-moved
  (fn [screen entities]
    (update! screen :mouse-x (:input-x screen)) ; the x position of the mouse
    (update! screen :mouse-y (:input-y screen)) ; the y position of the mouse
    entities))



(defgame breakout
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
