(ns au.net.ryansattler.main
  (:import
    (java.awt Color Dimension)
    (java.awt.event KeyListener)
    (javax.swing JFrame JOptionPane JPanel))
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE VK_SHIFT)

(def max-fps 30)
(def min-millis-per-frame (long (/ 1000 max-fps)))
(def window-width 800)
(def window-height 600)

(def wall-width 5)
(def path-width 10)

(def player
  {:score 0})

(defstruct wall-piece :x :y)

(defn gen-level []
  [(struct wall-piece 150 150) 
   (struct wall-piece 155 155) 
   (struct wall-piece 250 275)])

(def initial-gamestate {:mouseX 0
                        :mouseY 0
                        :levelnum 1
                        :level (gen-level)
                        :in-wall-piece nil})

(def images nil) ;map from item-image-type to render function? - need to render in right order too

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})


(defn render-background [gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 window-width window-height))

(defn render-debug [gfx mouseX mouseY in-wall-piece]
  ;(println frame)
  (.setColor gfx (color :black))
  (.drawString gfx (str "X: " mouseX) 50 50)
  (.drawString gfx (str "Y: " mouseY) 50 75)
  (if in-wall-piece
    (.drawString gfx "Collided!" 50 100)))

(defn render-level [gfx level]
  (.setColor gfx (color :black))
  (doseq [wall-piece level]
    (.fillRect gfx (:x wall-piece) (:y wall-piece) wall-width wall-width)))

(defn render [game window]
  (let [gfx (.getGraphics window)]
      (render-background gfx)
      (render-debug gfx (game :mouseX) (game :mouseY) (game :in-wall-piece))
      (render-level gfx (game :level))))

(defn update-mouse [gamestate]
  (let [pointerInfo (java.awt.MouseInfo/getPointerInfo)
        mousePoint (.getLocation pointerInfo)]
      (assoc gamestate 
        :mouseX (.x mousePoint)
        :mouseY (.y mousePoint))))

(defn in-wall-piece? [wall-piece mouseX mouseY]
  (let [wallX (wall-piece :x)
        wallY (wall-piece :y)]
    (and
      (>= mouseY wallY)
      (<= mouseY (+ wallY wall-width))
      (>= mouseX wallX)
      (<= mouseX (+ wallX wall-width)))))

(defn collided-piece [gamestate]
  (let [{:keys [mouseX mouseY level]} gamestate]
        (first (filter #(in-wall-piece? % mouseX mouseY) level))))

(defn update [game frame]
  (let [game (update-mouse game)]
    (assoc game :in-wall-piece (collided-piece game))))
 

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