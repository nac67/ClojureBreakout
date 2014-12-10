(ns breakout.game
  (:require [play-clj.g2d :refer :all]
            [breakout.collisions :as collisions]
            [breakout.utils :as u]
            [breakout.constants :as c]))


(defn addBricks [entities]
  (u/concatV entities 
     (map (fn [[x y]]
        (assoc (texture c/BRICK_PATH)
               :game-type c/BRICK
               :x (+ (* x 70) 200)
               :y (- 530 (* y 30))
               :w 60
               :h 20))
        (u/gen2DCoords 5 6))))


(defn initGame []
  (-> [(assoc (texture c/BALL_PATH) :game-type c/BALL :x c/H_G_WIDTH :y c/H_G_HEIGHT :vx 5 :vy 5 :w 20 :h 20)
       (assoc (texture c/PADDLE_PATH) :game-type c/PADDLE :x c/H_G_WIDTH :y 30 :w 100 :h 20)]
      (addBricks)))


(defn move [entity x y]
  (let [newX (+ (:x entity) x)
        newY (+ (:y entity) y)]
    (assoc entity :x newX :y newY)))


(defn bounce [entity]
  (let [newVx (if (u/outside? (:x entity) 0 c/G_WIDTH) (u/inv (:vx entity)) (:vx entity))
        newVy (if (> (:y entity) c/G_HEIGHT) (u/inv (:vy entity)) (:vy entity))]
    (assoc entity :vx newVx :vy newVy)))


(defn fallOffScreen [ball]
  (if (< (:y ball) -100)
             (assoc ball :x c/H_G_WIDTH :y c/H_G_HEIGHT)
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
                    (= (:game-type hitObject) c/BRICK)
                       (assoc hitObject :destroyed true) ; mark brick as destroyed
                    :default
                        hitObject))) ; if object not a brick, leave it alone
           ball (if collisions? (assoc ball :vy (u/inv (:vy ball))) ball)
           bricks (filter (fn [brick] (not (:destroyed brick))) bricks)]
      
      [ball bricks]))
    

(defn update [screen [ball paddle & bricks :as entities]]
  (let [newBall (updateBall ball)
        newPaddle (assoc paddle :x (- (:mouse-x screen) (-> paddle (:w) (/ 2))))
        [newBall2 newBricks] (processBreakoutCollisions newBall paddle bricks)]
    (u/concatV [newBall2 newPaddle] newBricks)))