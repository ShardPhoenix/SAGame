(ns au.net.ryansattler.graphics
  (:import
    (java.awt Color Dimension Graphics Graphics2D)
    (java.awt.image BufferedImage)
    (javax.swing JFrame JOptionPane JPanel))
  (:use au.net.ryansattler.constants))

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :green(Color. 61 245 0)
            :gold (Color. 255 255 61)
            :brown (Color. 102 51 0) 
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn render-background [#^Graphics gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 ( * 2 window-width) (* 2 window-height)))

(defn render-debug [#^Graphics gfx game frame]
  (println frame)
  (.setColor gfx (color :black))
  (.drawString gfx (str "player at " (game :playerx) " " (game :playery)) 75 125)
  (if (game :in-wall-piece)
    (.drawString gfx (str "Collided at " (game :in-wall-piece) :x) ", " ((game :in-wall-piece) :y) 50 100)))

(defn render-level [#^Graphics gfx level]
  (doseq [maze-cell level]
    (if (:wall maze-cell)
        (do
          (if (:touched maze-cell)
            (.setColor gfx (color :red))
            (.setColor gfx (color :black)))
          (.fillRect gfx (maze-cell :x) (maze-cell :y)  wall-width wall-width))
        (do
          (cond  
                (:treasure maze-cell) (.setColor gfx (color :gold))
                (:touched maze-cell)  (.setColor gfx (color :green))
            :else (.setColor gfx (color :background)))
          (.fillRect gfx (maze-cell :x) (maze-cell :y)  wall-width wall-width)))))

(defn render-player [#^Graphics gfx game]
  (.setColor gfx (color :blue))
  (.fillRect gfx (first (game :playerpos)) (second (game :playerpos)) wall-width wall-width))

(defn render-minotaur [#^Graphics gfx game]
  (.setColor gfx (color :brown))
  (.fillRect gfx (first (game :minotaurpos)) (second (game :minotaurpos)) wall-width wall-width))

(defn render [game window frame]
  (let [#^BufferedImage image (.createImage window window-width window-height)
        #^Graphics gfx (.createGraphics image)
        #^Graphics2D gfx2 (.getGraphics #^JFrame window)]
      (render-background gfx)
      (if debug
        (render-debug gfx game frame))
      (render-level gfx (game :level))
      (render-player gfx game)
      (render-minotaur gfx game)
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

