;TODO:
;- add actual maze-gen algo
;- separate into files (eg for graphics.
;- making graphics non-flickering (use buffered-image
;- add timelimit and win condition, lose condition
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, points for squares touched etc
;- no-cheat mouse interpolation
;- sounds
;- distributable package and/or applet

(ns au.net.ryansattler.main
  (:import
    (java.awt Color Dimension)
    (javax.swing JFrame JOptionPane JPanel)))

(def max-fps 30)
(def min-millis-per-frame (long (/ 1000 max-fps)))
(def window-width 1024)
(def window-height 768)

(def wall-width 15)
(def path-width 15)
(def maze-size 35) ;odd number
(def maze-top-margin 150)
(def maze-left-margin 300)

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

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :green(Color. 61 245 0)
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn render-background [gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 ( * 2 window-width) (* 2 window-height)))

(defn render-debug [gfx mouseX mouseY in-wall-piece]
  ;(println frame)
  (.setColor gfx (color :black))
  (.drawString gfx (str "X: " mouseX) 50 50)
  (.drawString gfx (str "Y: " mouseY) 50 75)
  (if in-wall-piece
    (.drawString gfx (str "Collided at " (in-wall-piece :x) ", " (in-wall-piece :y)) 50 100)))

(defn render-level [gfx level]
  (doseq [maze-cell level]
    (if (:wall maze-cell)
        (do
          (if (:touched maze-cell)
            (.setColor gfx (color :red))
            (.setColor gfx (color :black)))
          (.fillRect gfx (maze-cell :x) (maze-cell :y)  wall-width wall-width))
        (do
          (if (:touched maze-cell)
            (.setColor gfx (color :green))
            (.setColor gfx (color :background)))
          (.fillRect gfx (maze-cell :x) (maze-cell :y)  wall-width wall-width)))))

(defn render [game window]
  (let [gfx (.getGraphics window)]
      (render-background gfx)
      (render-debug gfx (game :mouseX) (game :mouseY) (game :in-wall-piece))
      (render-level gfx (game :level))))

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

(defn configure-gui [window panel]
  (doto panel
    (.setPreferredSize (Dimension. window-width window-height))
    (.setFocusable true))
  (doto window
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)))

(let [window (JFrame. "You Can't Touch The Walls")
      panel (javax.swing.JPanel.)]
  (configure-gui window panel)
  (loop [gamestate initial-gamestate
         frame 1]
    (let [start-time (current-time)
          updated-gamestate (update gamestate frame)]
         (render gamestate window)
    (let [render-time (- (current-time) start-time)
          wait-time (max (- min-millis-per-frame render-time) 0)]
      (java.lang.Thread/sleep wait-time))
    (recur updated-gamestate (inc frame)))))