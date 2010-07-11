;TODO:
;- add time limit and win condition, lose condition
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- powerups?
;- hardcore modes: 1 hit/life, narrower corridors, low time-limit
;- no-cheat mouse interpolation (if needed) & make sure green path actually connects
;- sounds/music
;- enemies?!
;- pause (+ free mouse) w/ keystroke - & move mouse back when unpaused?
;- click-and-drag?
;- distributable package and/or applet
;- tests
;- keyboard driven game with "you can't escape the minotaur" instead?!
;- fix collision
;- smooth animation?
;- "theseus and minotaur", treasures, no move through walls, touching walls (treasures, etc) alerts minotaur?

(ns au.net.ryansattler.main
  (:import
    (java.awt Dimension)
    (javax.swing JFrame JPanel)
    (java.awt.event KeyListener)
    (java.awt.event KeyEvent))
  (:use au.net.ryansattler.constants)
  (:use [au.net.ryansattler.graphics  :only (render configure-gui)])
  (:use [au.net.ryansattler.mazegen :only (gen-level)])
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE VK_SHIFT)

(if debug 
  (set! *warn-on-reflection* true))

(def player
  {:score 0
   :level-on 0})

(def initial-gamestate {:levelnum 1
                        :level (gen-level)
                        :playerx (- maze-left-margin wall-width)
                        :playery (-  maze-top-margin wall-width)
                        :collided-piece nil})

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(def last-moved (atom (current-time)))

(defn in-piece? [piece x y]
    (let [wallX (piece :x)
          wallY (piece :y)]
	    (and
	      (>= y wallY)
	      (<= y (+ wallY wall-width))
	      (>= x wallX)
	      (<= x (+ wallX wall-width)))))

(defn update-touched [level x y]
  (map #(if (in-piece? % (inc x) (inc y))
           (assoc % :touched true)
           %) 
    level))

(defn collided-piece [gamestate]
  (let [{:keys [playerx playery level]} gamestate]
        (first (filter #(and (:wall %) (in-piece? % playerx playery)) level))))

(defn try-move [frame playerpos direc]
  (let [time (current-time)]
	  (if (and (not (zero? direc)) (> (- time @last-moved) min-millis-per-move))
     (do
      (compare-and-set! last-moved @last-moved (current-time))
	    (+ playerpos (* wall-width direc)))
	    playerpos)))

(defn update [game input frame window]
  (assoc game :collided-piece (collided-piece game)
              :level (update-touched (game :level) (game :playerx) (game :playery))
              :playerx (try-move frame (game :playerx) (input :x-direc))
              :playery (try-move frame (game :playery) (input :y-direc))))

(defn get-input [keys-set] 
  (let [left (if (keys-set VK_LEFT) -1 0)
        right (if (keys-set VK_RIGHT) 1 0)
        up (if (keys-set VK_UP) -1 0)
        down (if (keys-set VK_DOWN) 1 0)]
        {:x-direc (+ left right) 
         :y-direc (+ up down) 
         :fire? (or (keys-set VK_SPACE) (keys-set VK_SHIFT))}))

(defn create-panel [width height key-code-atom]
  (proxy [JPanel KeyListener]
    [] ; superclass constructor arguments
    (getPreferredSize [] (Dimension. width height))
    (keyPressed [#^KeyEvent e]
      (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom (.getKeyCode e))))
    (keyReleased [#^KeyEvent e]
      (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom (.getKeyCode e))))
    (keyTyped [e]) ; do nothing
    ))

(let [window (JFrame. "You Can't Touch The Walls")
      keys-set-atom (atom #{}) ;set of keyboard keys currently being held down by player
      panel (create-panel window-width window-height keys-set-atom)]
  (configure-gui window panel)
  (loop [gamestate initial-gamestate
         frame 1]
    (let [start-time (current-time)
          input (get-input @keys-set-atom)
          updated-gamestate (update gamestate input frame window)]
         (render gamestate window frame)
    (let [render-time (- (current-time) start-time)
          wait-time (max (- min-millis-per-frame render-time) 0)]
      (if debug
        (println (double render-time)))
      (java.lang.Thread/sleep wait-time))
    (recur updated-gamestate (inc frame)))))