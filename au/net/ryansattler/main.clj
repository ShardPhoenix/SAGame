;TODO:
;- add actual maze-gen algo
;- add time limit and win condition, lose condition
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- no-cheat mouse interpolation (if needed)
;- sounds
;- distributable package and/or applet

(ns au.net.ryansattler.main
  (:import
    (javax.swing JFrame JPanel))
  (:use au.net.ryansattler.constants)
  (:use [au.net.ryansattler.graphics  :only (render configure-gui)])
  (:use [au.net.ryansattler.mazegen :only (gen-level)]))

(if debug 
  (set! *warn-on-reflection* true))

(def player
  {:score 0
   :level-on 0})

(def initial-gamestate {:mouseX 0
                        :mouseY 0
                        :levelnum 1
                        :level (gen-level)
                        :collided-piece nil})

;moved one pixel to the right - seems to feel more accurate that way?
(defn get-mouseX []
  (inc (.x (.getLocation (java.awt.MouseInfo/getPointerInfo)))))

(defn get-mouseY []
  (.y (.getLocation (java.awt.MouseInfo/getPointerInfo))))

(defn in-piece? [piece mouseX mouseY]
    (let [wallX (piece :x)
          wallY (piece :y)]
	    (and
	      (>= mouseY wallY)
	      (<= mouseY (+ wallY wall-width))
	      (>= mouseX wallX)
	      (<= mouseX (+ wallX wall-width)))))

(defn update-touched [level mouseX mouseY]
  (map #(if (in-piece? % mouseX mouseY)
           (assoc % :touched true)
           %) 
    level))

(defn collided-piece [gamestate]
  (let [{:keys [mouseX mouseY level]} gamestate]
        (first (filter #(and (:wall %) (in-piece? % mouseX mouseY)) level))))

(defn update [game frame]
  (assoc game :collided-piece (collided-piece game)
              :level (update-touched (game :level) (get-mouseX) (get-mouseY))
              :mouseX (get-mouseX)
              :mouseY (get-mouseY)))

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(let [window (JFrame. "You Can't Touch The Walls")
      panel (JPanel.)]
  (configure-gui window panel)
  (loop [gamestate initial-gamestate
         frame 1]
    (let [start-time (current-time)
          updated-gamestate (update gamestate frame)]
         (render gamestate window frame)
    (let [render-time (- (current-time) start-time)
          wait-time (max (- min-millis-per-frame render-time) 0)]
      (if debug
        (println (double render-time)))
      (java.lang.Thread/sleep wait-time))
    (recur updated-gamestate (inc frame)))))