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

(defn coord-to-pix [[col row]]
  [(+ maze-left-margin (* col wall-width)) (+  maze-top-margin (* row wall-width))])

(defn render-background [#^Graphics gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 ( * 2 window-width) (* 2 window-height)))

(defn render-debug [#^Graphics gfx game frame]
  ;(println frame)
  (.setColor gfx (color :black)))

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

(defn render-square [#^Graphics gfx thecolor [x y]]
  (.setColor gfx (color thecolor))
  (.fillRect gfx x y wall-width wall-width))

(defn render-treasures [gfx treasures]
  (dotimes [n treasures]
    (render-square gfx :gold (coord-to-pix [(+ (* 1.5 n) -10) -5]))))

(defn render-route [gfx route]
  (doseq [coord route]
    (render-square gfx :red (coord-to-pix coord)))) 

(defn render [game window frame]
  (let [#^BufferedImage image (.createImage window window-width window-height)
        #^Graphics gfx (.createGraphics image)
        #^Graphics2D gfx2 (.getGraphics #^JFrame window)]
      (render-background gfx)
      (if debug
        (render-debug gfx game frame))
      (render-level gfx (game :level))
      (render-route gfx (game :route)) 
      (render-square gfx :blue (coord-to-pix (game :playerpos)))
      (render-square gfx :brown (coord-to-pix (game :minotaurpos)))
      (render-treasures gfx (game :treasures-gained)) 
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

