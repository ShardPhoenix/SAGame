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

(def player
  {:score 0})

(def initial-gamestate {:mouseX 0
                        :mouseY 0
                        :level 1})


(def images nil) ;map from item-image-type to render function? - need to render in right order too

(def color {:blue (Color. 0 61 245)
            :red  (Color. 245 61 0)
            :black (Color. 0 0 0)
            :background (Color. 255 255 255)})

(defn render-background [gfx] 
    (.setColor gfx (color :background))
    (.fillRect gfx 0 0 window-width window-height))

(defn render-debug [gfx mouseX mouseY]
  (.setColor gfx (color :black))
  (.drawString gfx (str "X: " mouseX) 100 100)
  (.drawString gfx (str "Y: " mouseY) 100 125))

(defn render [game window]
  (let [gfx (.getGraphics window)]
      (render-background gfx)
      (render-debug gfx (game :mouseX) (game :mouseY))))

(defn update-mouse [gamestate]
  (let [pointerInfo (java.awt.MouseInfo/getPointerInfo)
        mousePoint (.getLocation pointerInfo)
        newMouseX (.x mousePoint)
        newMouseY (.y mousePoint)]
      (assoc gamestate 
        :mouseX newMouseX
        :mouseY newMouseY)))

(defn update [input game frame]
  (println frame)
  (update-mouse game))

(defn get-input [keys-set] 
  (let [left (if (keys-set VK_LEFT) -1 0)
        right (if (keys-set VK_RIGHT) 1 0)
        up (if (keys-set VK_UP) -1 0)
        down (if (keys-set VK_DOWN) 1 0)]
        {:x-direc (+ left right) 
         :y-direc (+ up down) 
         :fire? (or (keys-set VK_SPACE) (keys-set VK_SHIFT))}))

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(defn create-panel [width height key-code-atom]
  (proxy [JPanel KeyListener]
    [] ; superclass constructor arguments
    (getPreferredSize [] (Dimension. width height))
    (keyPressed [e]
      (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom (.getKeyCode e))))
    (keyReleased [e]
      (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom (.getKeyCode e))))
    (keyTyped [e]) ; do nothing
    ))

(defn configure-gui [window panel]
  (doto panel
    (.setFocusable true) ; won't generate key events without this
    (.addKeyListener panel))
  (doto window
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)))

(let [window (JFrame. "You Can't Touch The Walls")
      keys-set-atom (atom #{}) ;set of keyboard keys currently being held down by player
      panel (create-panel window-width window-height keys-set-atom)]
  (configure-gui window panel)
  (loop [gamestate initial-gamestate
         frame 1]
    (let [input (get-input @keys-set-atom)
          start-time (current-time)]
    (let [updated-gamestate (update input gamestate frame)]
         (render gamestate window)
    (let [render-time (- (current-time) start-time)
          wait-time (max (- min-millis-per-frame render-time) 0)]
      (java.lang.Thread/sleep wait-time))
    (recur updated-gamestate (inc frame))))))




