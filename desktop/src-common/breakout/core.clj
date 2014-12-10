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
    
; TODO make a generic processCollisions function that
; takes in an object, and a list of other objects
; it should also take in a function
; if a collision is detected, it runs this function on the object in the list
; it returns true if there was a collision, and it returns the new list with
; the functions applied to those who were collided
(defn processCollisions [ball paddle bricks]
    (let [bricks (map (fn [brick] 
                        (if (colliding? ball brick) 
                          (assoc brick :destroyed true) 
                          brick))
                      bricks)
          bounce (some (fn [brick] (colliding? ball brick)) bricks)
          bounce (or bounce (colliding? ball paddle))
          newVy (if bounce (inv (:vy ball)) (:vy ball))]
       
    [(assoc ball :vy newVy)
     (filter (fn [brick] (not (:destroyed brick))) bricks)]))

(defn update [screen [ball paddle & bricks :as entities]]
  (let [newBall (updateBall ball)
        newPaddle (assoc paddle :x (- (:mouse-x screen) 40))
        [newBall2 newBricks] (processCollisions newBall paddle bricks)]
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
