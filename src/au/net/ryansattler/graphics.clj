(ns au.net.ryansattler.graphics
  (:import
    (java.awt Color Dimension Graphics Graphics2D)
    (java.awt.image BufferedImage)
    (javax.swing JFrame JOptionPane JPanel)
    (javax.imageio ImageIO)
    (java.io File FileInputStream ByteArrayInputStream)
    (sun.audio AudioStream AudioPlayer))
  (:use au.net.ryansattler.constants)
  (:use [clojure.contrib.duck-streams :only (to-byte-array)]))

(def minotaur-image (ImageIO/read (File. "images/mino.png")))
(def player-image (ImageIO/read (File. "images/hero.png")))
(def treasure-image (ImageIO/read (File. "images/gold.png")))
(def wall-image (ImageIO/read (File. "images/wall.png")))
(def bomb-image (ImageIO/read (File. "images/bomb2.png")))
(def floor-image (ImageIO/read (File. "images/dirt.png")))
(def title-image (ImageIO/read (File. "images/title.png"))) 
(def loss-image (ImageIO/read (File. "images/died.png"))) 

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :green(Color. 61 245 0)
            :gold (Color. 255 255 0)
            :brown (Color. 102 51 0) 
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn load-sound [filename]
  (to-byte-array (FileInputStream. filename))) 

(def sounds {:roar1 (load-sound "sounds/roar1.wav")
             :roar2 (load-sound "sounds/roar2.wav")
             :ching (load-sound "sounds/ching.wav")
             :scream (load-sound "sounds/scream.wav")
             :reload (load-sound "sounds/reload2.wav")
             :explosion (load-sound "sounds/explosion.wav")})

(defn play-sound [sound]
  (let [audiostream (AudioStream. (ByteArrayInputStream. (sounds sound)))] 
    (if debug (println "playing" sound))
    (.start AudioPlayer/player audiostream)))

(defn play-sounds [events]
  (cond (events :got-treasure) (play-sound :ching)
        (events :got-bomb) (play-sound :reload)
        (events :bombed) (play-sound :explosion)
        (events :minotaur-started) (play-sound :roar1)))

(defn coord-to-pix [[col row]]
  [(int (+ maze-left-margin (* col wall-width))) 
   (int (+ maze-top-margin (* row wall-width)))])

(defn render-background [#^Graphics gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 ( * 2 window-width) (* 2 window-height)))

(defn render-level [#^Graphics gfx window level levelnum]
 (let [maze-background-color  
    (Color/getHSBColor 0 (min 1.0 (* 0.1 (dec levelnum))) 1.0)]
  (doseq [maze-cell level]
    (.drawImage gfx floor-image (maze-cell :x) (maze-cell :y) window)
    (cond (:wall maze-cell) (.drawImage gfx wall-image (maze-cell :x) (maze-cell :y) window)
          (:treasure maze-cell) (.drawImage gfx treasure-image (maze-cell :x) (maze-cell :y) window)
          (:bomb-pickup maze-cell) (.drawImage gfx bomb-image (maze-cell :x) (maze-cell :y) window)))))

(defn render-square [#^Graphics gfx thecolor [x y]]
  (.setColor gfx (color thecolor))
  (.fillRect gfx x y wall-width wall-width))

(defn render-treasures [gfx window treasures]
  (dotimes [n treasures]
    (let [[x y] (coord-to-pix [(+ (* 1.5 n) 0) -4])]
      (.drawImage gfx treasure-image x y window))))

(defn render-bombs [gfx window bombs]
  (dotimes [n bombs]
    (let [[x y] (coord-to-pix [(+ (* 1.5 n) 0) -2])]
      (.drawImage gfx bomb-image x y window))))

(defn render-route [gfx route]
  (doseq [coord route]
    (render-square gfx :red (coord-to-pix coord))))

(defn render-scores [gfx game]
  (.setColor gfx (color :black))
  (.drawString gfx (str "Level " (:levelnum game)) 75 75)
  (.drawString gfx (str "Total treasures: " (:total-treasures game)) 75 100)
  (.drawString gfx (str "Total score: " (:score game)) 75 125)
  (.drawString gfx (str "Bombs left: " (:bombs (:player game))) 75 150))

(defn render-debug [#^Graphics gfx game frame])
  ;(render-route gfx (:route (game :minotaur))))

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

(defn render-loss-screen [gfx window game]
 (let [treasures (:treasures-gained game)
       score-gained (* treasure-score-constant treasures treasures)
       total-treasures (+ treasures (:total-treasures game))
       spacing 25
       y-pos (int (/ window-width 3))
       x-pos (int (* 0.40 window-height))]
  (play-sound :roar2)
  (java.lang.Thread/sleep 250)
  (play-sound :scream) ;& fade to red?
  (java.lang.Thread/sleep 2000) 
  (.setColor gfx (color :black))
  (.drawImage gfx loss-image (int (/ (- window-width 504) 2)) (int (* 0.30 window-height)) window)
  (.drawString gfx "You died with: " (- y-pos 33) (+ (* spacing 0) x-pos))
  (.drawString gfx (str total-treasures " treasures") y-pos (+ (* spacing 1) x-pos))
  (.drawString gfx (str (dec (:levelnum game)) " levels escaped") y-pos (+ (* spacing 2) x-pos))
  (.drawString gfx (str (+ score-gained (:score game)) " points") y-pos (+ (* spacing 3) x-pos))
  (.drawString gfx "Starting new game..." y-pos (+ (* spacing 6) x-pos))))

(defn render-splash-screen [gfx window game]
  (render-background gfx) 
  (.setColor gfx (color :background))
  (.drawString gfx "" 0 0) ;work-around for image not otherwise appearing for unknown reason
  (.drawImage gfx title-image (int (/ (- window-width 521) 2)) (int (* 0.30 window-height)) window))

(defn render-instructions [gfx window]
 (let [left-margin 25
       top-margin 200
       spacing 25]
  (.setColor gfx (color :black))
  (.drawString gfx "Advice for those trying to escape the minotaur:" left-margin top-margin)

  (.drawString gfx "You are: " left-margin (+ (* 1 spacing) top-margin))
  (.drawImage gfx player-image (int (+ 50 left-margin)) (int (+ (* 0.5 spacing) top-margin)) window)

  (.drawString gfx "Use arrow keys to move" left-margin (+ (* 2 spacing) top-margin))

  (.drawString gfx "Use Ctrl or Space to        the walls around you" left-margin (+ (* 3 spacing) top-margin))
  (.drawImage gfx bomb-image (int (+ 112 left-margin)) (int (+ (* 2.5 spacing) top-margin)) window)

  (.drawString gfx "Collect more        from the level" left-margin (+ (* 4 spacing) top-margin))
  (.drawImage gfx bomb-image (int (+ 74 left-margin)) (int (+ (* 3.5 spacing) top-margin)) window)

  (.drawString gfx "Collect        for points, then escape at the bottom right" left-margin (+ (* 5 spacing) top-margin))
  (.drawImage gfx treasure-image (int (+ 42 left-margin)) (int (+ (* 4.5 spacing) top-margin)) window)  

  (.drawString gfx "Don't get eaten by: " left-margin (+ (* 6 spacing) top-margin))
  (.drawImage gfx minotaur-image (int (+ 108 left-margin)) (int (+ (* 5.5 spacing) top-margin)) window) 
 
  (.drawString gfx "Press p to pause" left-margin (+ (* 8 spacing) top-margin))
  (.drawString gfx "Press h to hide these instructions" left-margin (+ (* 9 spacing) top-margin))))

;smoothly animate by interpolating between previous and current coords depending on speed
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

(defn render-paused [gfx window game]
  (render-scores gfx game)
  (if-not (:hidden game) 
    (render-instructions gfx window))
  (render-treasures gfx window (game :treasures-gained))
  (render-bombs gfx window (:bombs (game :player)))
  (.setColor gfx (color :black))
  (.drawString gfx (str "Game paused. Press p to resume.") (/ window-width 2) (/ window-height 2)))

(defn render [game window frame]
  (let [#^BufferedImage image (.createImage window window-width window-height)
        #^Graphics gfx (.createGraphics image)
        #^Graphics2D gfx2 (.getGraphics #^JPanel window)
        victory (:victory game)
        started (:started game)]
      (render-background gfx)
      (cond (not started) (render-splash-screen gfx window game) 
            (pos? victory) (render-victory-screen gfx game)
            (neg? victory) (render-loss-screen gfx window game)
            (:paused game) (render-paused gfx window game) 
					  :else  (do 
                     (if debug
							         (render-debug gfx game frame))
                     (play-sounds (game :sound-events))
                     (if-not (:hidden game) 
                       (render-instructions gfx window))
							       (render-level gfx window (game :level) (game :levelnum))
                     (render-image-smoothly gfx window minotaur-image (game :minotaur))
                     (render-image-smoothly gfx window player-image (game :player))
							       (render-treasures gfx window (game :treasures-gained))
                     (render-bombs gfx window (:bombs (game :player)))
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

