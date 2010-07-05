(ns au.net.ryansattler.graphics
  (:import
    (java.awt Color Dimension Graphics)
    (javax.swing JFrame JOptionPane JPanel))
  (:use au.net.ryansattler.constants))

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :green(Color. 61 245 0)
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn render-background [#^Graphics gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 ( * 2 window-width) (* 2 window-height)))

(defn render-debug [#^Graphics gfx frame mouseX mouseY in-wall-piece]
  (println frame)
  (.setColor gfx (color :black))
  (.drawString gfx (str "X: " mouseX) 50 50)
  (.drawString gfx (str "Y: " mouseY) 50 75)
  (if in-wall-piece
    (.drawString gfx (str "Collided at " (in-wall-piece :x) ", " (in-wall-piece :y)) 50 100)))

(defn render-level [#^Graphics gfx level]
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

(defn render [game window frame]
  (let [gfx (.getGraphics #^JFrame window)]
      (render-background gfx)
      (if debug
        (render-debug gfx frame (game :mouseX) (game :mouseY) (game :in-wall-piece)))
      (render-level gfx (game :level))))

(defn configure-gui [#^JFrame window #^JPanel panel]
  (doto panel
    (.setPreferredSize (Dimension. window-width window-height))
    (.setFocusable true))
  (doto window
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)))

