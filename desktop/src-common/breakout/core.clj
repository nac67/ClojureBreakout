(ns breakout.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [breakout.collisions :as collisions]
            [breakout.utils :as u]))

(def ball_path "ball.png")
(def brick_path "brick.png")
(def paddle_path "paddle.png")




; given x dimension and y dimension, outputs list of tuples in form
; [[0 0] [0 1] [0 2] [0 3] ... [1 0] [1 1] [1 2]...[x-1 y-1]]


(defn addBricks [entities]
  (u/concatV entities 
     (map (fn [[x y]]
        (assoc (texture brick_path)
               :game-type "brick"
               :x (+ (* x 70) 200)
               :y (- 530 (* y 30))
               :w 60
               :h 20))
        (u/gen2DCoords 5 6))))

(defn initGame []
  (-> [(assoc (texture ball_path) :game-type "ball" :x 400 :y 300 :vx 5 :vy 5 :w 20 :h 20)
       (assoc (texture paddle_path) :game-type "paddle" :x 400 :y 30 :w 100 :h 20)]
      (addBricks)))


  


(defn move [entity x y]
  (let [newX (+ (:x entity) x)
        newY (+ (:y entity) y)]
    (assoc entity :x newX :y newY)))

(defn bounce [entity]
  (let [newVx (if (u/outside? (:x entity) 0 800) (u/inv (:vx entity)) (:vx entity))
        newVy (if (> (:y entity) 600) (u/inv (:vy entity)) (:vy entity))]
    (assoc entity :vx newVx :vy newVy)))

(defn fallOffScreen [ball]
  (if (< (:y ball) -100)
             (assoc ball :x 400 :y 300)
             ball))

(defn updateBall [ball]
  (-> ball (move (:vx ball) (:vy ball)) 
           (bounce)
           (fallOffScreen)))

    
(defn processBreakoutCollisions [ball paddle bricks]
    (let [[collisions? [paddle & bricks]] 
            (collisions/processCollisions 
              ball 
              (cons paddle bricks) ; collide ball against paddle or bricks
              (fn [hitObject]
                 (cond
                    (= (:game-type hitObject) "brick")
                       (assoc hitObject :destroyed true) ; mark brick as destroyed
                    :default
                        hitObject))) ; if object not a brick, leave it alone
           ball (if collisions? (assoc ball :vy (u/inv (:vy ball))) ball)
           bricks (filter (fn [brick] (not (:destroyed brick))) bricks)]
      
      [ball bricks]))
    

(defn update [screen [ball paddle & bricks :as entities]]
  (let [newBall (updateBall ball)
        newPaddle (assoc paddle :x (- (:mouse-x screen) 40))
        [newBall2 newBricks] (processBreakoutCollisions newBall paddle bricks)]
    (u/concatV [newBall2 newPaddle] newBricks)))

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
