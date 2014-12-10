(ns breakout.core
  (:require [play-clj.core :refer :all]
            [breakout.game :as game]))


(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (game/initGame))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen (game/update screen entities)))
  
  :on-mouse-moved
  (fn [screen entities]
    (update! screen :mouse-x (:input-x screen)) ; the x position of the mouse
    (update! screen :mouse-y (:input-y screen)) ; the y position of the mouse
    entities))


(defgame breakout
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
