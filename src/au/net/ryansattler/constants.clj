(ns au.net.ryansattler.constants)

(def debug true)

(def max-fps 60)
(def min-millis-per-frame (long (/ 1000 max-fps)))
(def window-width 1024)
(def window-height 768)
(def end-screen-time 5000)
(def start-screen-time 50) 

(def wall-width 16) ;pixels
(def maze-size 25) ;odd number
(def maze-top-margin 150)
(def maze-left-margin 450)

(def initial-player-millis-per-move 66)
(def initial-minotaur-millis-per-move 200)
(def minotaur-speed-up 0.9)
(def minotaur-start-delay 4000) 
(def bomb-delay 500)
(def starting-bombs 2) 

(def num-treasures 8) 
(def treasure-score-constant 50) 

