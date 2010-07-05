;TODO:
;- add actual maze-gen algo
;- separate into files (eg for graphics, maze-gen part)
;- making graphics non-flickering (use buffered-image)
;- add time limit and win condition, lose condition
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- no-cheat mouse interpolation (if needed)
;- sounds
;- distributable package and/or applet

(ns au.net.ryansattler.main
  (:import
    (javax.swing JFrame JPanel))
  (:use au.net.ryansattler.graphics)
  (:use au.net.ryansattler.constants))

(if debug 
  (set! *warn-on-reflection* true))

(def player
  {:score 0
   :level-on 0})

(defstruct maze-cell :col :row :x :y :wall :visited :touched)
(defn make-maze-cell [col row wall?] 
  (struct maze-cell 
	  col 
	  row 
	  (+ maze-left-margin (* wall-width col)) 
	  (+ maze-top-margin (* wall-width row))
	  wall?
	  false
	  false))

(defn initial-maze []
  (for [x (range maze-size) y (range maze-size)] 
    (make-maze-cell x y (or (zero? (rem x 2)) (zero? (rem y 2))))))

(defn gen-level2 [maze]
  (for [cell maze]
    (assoc cell :wall (> (Math/random) 0.5))))

(defn gen-level []
  (gen-level2 (initial-maze)))

(def initial-gamestate {:mouseX 0
                        :mouseY 0
                        :levelnum 1
                        :level (gen-level)
                        :in-wall-piece nil})

(defn get-mouseX []
  (.x (.getLocation (java.awt.MouseInfo/getPointerInfo))))

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
  (assoc game :in-wall-piece (collided-piece game)
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