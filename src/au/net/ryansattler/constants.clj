(ns au.net.ryansattler.constants)

(def debug true)

(def max-fps 30)
(def min-millis-per-frame (long (/ 1000 max-fps)))
(def window-width 850)
(def window-height 600)
(def end-screen-time 6000)
(def start-screen-time 3000) 

(def wall-width 16) ;pixels, should stay on 16 now that graphics are used
(def maze-size 25) ;odd number
(def maze-top-margin 100)
(def maze-left-margin 350)

(def initial-player-millis-per-move 66)
(def bomb-delay 500)
(def starting-bombs 2)
(def bombs-per-level 1)
(def num-bomb-pickups 1) 

(def initial-minotaur-millis-per-move 200)
(def minotaur-speed-up 0.9)
(def minotaur-start-delay 4000) 
 

(def num-treasures 8) 
(def treasure-score-constant 50) 

