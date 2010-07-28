(ns au.net.ryansattler.graphics
  (:import
    (java.awt Color Dimension Graphics Graphics2D)
    (java.awt.image BufferedImage)
    (javax.swing JFrame JOptionPane JPanel)
    (javax.imageio ImageIO)
    (java.io File))
  (:use au.net.ryansattler.constants))

;should place images in top level
(def minotaur-image (ImageIO/read (File. "images/mino.bmp")))


(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :green(Color. 61 245 0)
            :gold (Color. 255 255 0)
            :brown (Color. 102 51 0) 
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn coord-to-pix [[col row]]
  [(+ maze-left-margin (* col wall-width)) (+  maze-top-margin (* row wall-width))])

(defn render-background [#^Graphics gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 ( * 2 window-width) (* 2 window-height)))

(defn render-level [#^Graphics gfx level]
  (doseq [maze-cell level]
    (if (:wall maze-cell)
      (do
        (.setColor gfx (color :black))
        (.fillRect gfx (maze-cell :x) (maze-cell :y)  wall-width wall-width))
      (do
        (cond  
            (:treasure maze-cell) (.setColor gfx (color :gold))
            (:touched maze-cell)  (.setColor gfx (color :green))
            :else (.setColor gfx (color :background)))
        (.fillRect gfx (maze-cell :x) (maze-cell :y)  wall-width wall-width)))))

(defn render-square [#^Graphics gfx thecolor [x y]]
  (.setColor gfx (color thecolor))
  (.fillRect gfx x y wall-width wall-width))

(defn render-treasures [gfx treasures]
  (dotimes [n treasures]
    (render-square gfx :gold (coord-to-pix [(+ (* 1.5 n) -9) -5]))))

(defn render-route [gfx route]
  (doseq [coord route]
    (render-square gfx :red (coord-to-pix coord))))

(defn render-scores [gfx game]
  (.setColor gfx (color :black))
  (.drawString gfx (str "Level " (:levelnum game)) 75 75)
  (.drawString gfx (str "Total treasures: " (:total-treasures game)) 75 100)
  (.drawString gfx (str "Total score: " (:score game)) 75 125)
  (.drawString gfx (str "Bombs left: " (:bombs (:player game))) 75 150))

(defn render-debug [#^Graphics gfx game frame]
  (render-route gfx (:route (game :minotaur))))

(defn render-victory-screen [gfx game]
 (let [left-margin (/ window-width 4)
       top-margin (/ window-height 2)
       spacing 25
       treasures (:treasures-gained game)
       score-gained (* treasure-score-constant treasures treasures)]
  (.setColor gfx (color :black))
  (.drawString gfx (str "You escaped with " treasures " treasures!") left-margin top-margin)
  (.drawString gfx (str treasures " treasures gives " score-gained " points!") left-margin (+ spacing top-margin))
  (.drawString gfx (str "You now have " (+ score-gained (:score game)) " points in total") left-margin (+ (* 2 spacing) top-margin))
  (.setColor gfx (color :red)) 
  (.drawString gfx (str "The minotaur is getting angrier!") left-margin (+ (* 4 spacing) top-margin))))

(defn render-loss-screen [gfx game]
 (let [treasures (:treasures-gained game)
       score-gained (* treasure-score-constant treasures treasures)]
  (.setColor gfx (color :black))
  (.drawString gfx (str "You couldn't escape the minotaur! You died with " (+ (:treasures-gained game) (:total-treasures game)) 
                        " treasures and " (dec (:levelnum game)) " levels escaped, and") (/ window-width 4) (/ window-height 2))
  (.drawString gfx (str (+ score-gained (:score game)) " points.") (/ window-width 4) (+ 25 (/ window-height 2)))))

(defn render-splash-screen [gfx game]
  (.setColor gfx (color :black))
  (.drawString gfx (str "You Can't Escape the Minotaur!... but you can try!")
    (/ window-width 4) (/ window-height 2)))

(defn render-instructions [gfx]
 (let [left-margin 25
       top-margin 200
       spacing 25]
  (.setColor gfx (color :black))
  (.drawString gfx "Advice for those trying to escape the minotaur:" left-margin top-margin)
  (.drawString gfx "You are: " left-margin (+ (* 1 spacing) top-margin))
  (render-square gfx :blue [(+ 50 left-margin) (+ (* 0.5 spacing) top-margin)])
  (.setColor gfx (color :black))
  (.drawString gfx "Use arrow keys to move" left-margin (+ (* 2 spacing) top-margin))
  (.drawString gfx "Use Ctrl to bomb the walls around you (two per level)" left-margin (+ (* 3 spacing) top-margin))
  (.drawString gfx "Collect treasures for points, then escape at the bottom right" left-margin (+ (* 4 spacing) top-margin))
  (.drawString gfx "Don't get eaten by: " left-margin (+ (* 5 spacing) top-margin))
  (render-square gfx :brown [(+ 108 left-margin) (+ (* 4.5 spacing) top-margin)])
  (.setColor gfx (color :black))
  (.drawString gfx "Press p to pause" left-margin (+ (* 7 spacing) top-margin))))

;smoothly animate by interpolating between previous and current coords depending on speed
(defn render-smoothly [gfx color unit]
 (let [coord (:coord unit)
       last-coord (:last-coord unit)
       last-moved (:last-moved unit)
       millis-per-move (:millis-per-move unit) 
       thetime (current-time)
       time-since-moved (- thetime last-moved) 
       x-diff (- (first coord) (first last-coord))
       y-diff (- (second coord) (second last-coord))
       x-delta (* x-diff (min 1 (/ time-since-moved millis-per-move)))
       y-delta (* y-diff (min 1 (/ time-since-moved millis-per-move)))
       coord-to-draw [(+ (first last-coord) x-delta)
                      (+ (second last-coord) y-delta)]] 
  (render-square gfx color (coord-to-pix coord-to-draw))))

;refactor plz
(defn render-image-smoothly [gfx window image unit]
  (let [coord (:coord unit)
       last-coord (:last-coord unit)
       last-moved (:last-moved unit)
       millis-per-move (:millis-per-move unit) 
       thetime (current-time)
       time-since-moved (- thetime last-moved) 
       x-diff (- (first coord) (first last-coord))
       y-diff (- (second coord) (second last-coord))
       x-delta (* x-diff (min 1 (/ time-since-moved millis-per-move)))
       y-delta (* y-diff (min 1 (/ time-since-moved millis-per-move)))
       coord-to-draw [(+ (first last-coord) x-delta)
                      (+ (second last-coord) y-delta)]] 
  (.drawImage gfx image (int (first (coord-to-pix coord-to-draw))) 
                        (int (second (coord-to-pix coord-to-draw))) window)))

(defn render-paused [gfx game]
  (render-scores gfx game)
  (render-instructions gfx) 
  (render-treasures gfx (game :treasures-gained))
  (.setColor gfx (color :black))
  (.drawString gfx (str "Game paused. Press p to resume.") (/ window-width 2) (/ window-height 2)))

(defn render [game window frame]
  (let [#^BufferedImage image (.createImage window window-width window-height)
        #^Graphics gfx (.createGraphics image)
        #^Graphics2D gfx2 (.getGraphics #^JPanel window)
        victory (:victory game)
        started (:started game)]
      (render-background gfx)
      (cond (not started) (render-splash-screen gfx game) 
            (pos? victory) (render-victory-screen gfx game)
            (neg? victory) (render-loss-screen gfx game)
            (:paused game) (render-paused gfx game) 
					  :else  (do 
                     (if debug
							         (render-debug gfx game frame))
                     (render-instructions gfx)
							       (render-level gfx (game :level))
							       (render-smoothly gfx :blue (game :player)) 
							       ;(render-smoothly gfx :brown (game :minotaur))
                     (render-image-smoothly gfx window minotaur-image (game :minotaur))
							       (render-treasures gfx (game :treasures-gained))
							       (render-scores gfx game)))
      (.drawImage gfx2 image 0 0 window)))

(defn configure-gui [#^JFrame window #^JPanel panel]
  (doto panel
    (.addKeyListener panel)
    (.setFocusable true))
  (doto window
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)))

