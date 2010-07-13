;TODO:
;- add time limit and win condition, lose condition
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- powerups?
;- hardcore modes: 1 hit/life, narrower corridors, low time-limit
;- sounds/music
;- start, pause buttons
;- click-and-drag?
;- distributable package and/or applet
;- tests
;- keyboard driven game with "you can't escape the minotaur" instead?!
;- smooth animation?
;- "theseus and minotaur", treasures, no move through walls, touching walls (treasures, etc) alerts minotaur?
;- diff. music after minotaur is activiated
;- goal: grab all treasures and escape, don't get caught by minotaur
;- don't allow walking through walls
;- time limit after which minotaur goes straight for you?
;- "you can't bump the walls" or "you can't escape the minotaur" or..?

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

(defn find-minotaur [level]
  (let [minotaur-start (first (filter #(true? (:minotaur-start %)) level))]
    [(:col minotaur-start) (:row minotaur-start)]))

(defn initial-gamestate [] 
  (let [level (gen-level)]
	  {:levelnum 1
	  :level level
	  ;change to row/col and calc this when rendering?
	  :playerpos [1 1]
	  :minotaurpos (find-minotaur level)
	  :collided-piece nil}))

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(def last-moved (atom (current-time)))

;fix weird collision requiring inc?
(defn in-piece? [piece [col row]]
    (and (= (:col piece) col)
         (= (:row piece) row))) 

(defn update-touched [level coord]
  (map #(if (in-piece? % coord)
           (assoc % :touched true)
           %) 
    level))

(defn is-in-wall? [coord level]
  (pos? (count (filter true? (map #(and (:wall %) (in-piece? % coord)) level)))))

;add wall-bumping somehow - multiple returns?
(defn try-move [[col row] x-direc y-direc level]
  (let [time (current-time)
        newcoord [(+ col x-direc) 
                  (+ row y-direc)]]
	  (if (and (not (and (zero? x-direc) (zero? y-direc))) 
             (> (- time @last-moved) min-millis-per-move)
             (not (is-in-wall? newcoord level)))
     (do
      (compare-and-set! last-moved @last-moved (current-time))
	     newcoord)
	    [col row])))

(defn update [game input frame window]
  (assoc game :level (update-touched (game :level) (game :playerpos))
              :playerpos (try-move (game :playerpos) (input :x-direc) (input :y-direc) (game :level))))

(defn get-input [keys-set] 
  (let [left (if (keys-set VK_LEFT) -1 0)
        right (if (keys-set VK_RIGHT) 1 0)
        up (if (keys-set VK_UP) -1 0)
        down (if (keys-set VK_DOWN) 1 0)]
        {:x-direc (+ left right) 
         :y-direc (+ up down)}))

(defn create-panel [width height key-code-atom]
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. width height))
    (keyPressed [#^KeyEvent e]
      (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom (.getKeyCode e))))
    (keyReleased [#^KeyEvent e]
      (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom (.getKeyCode e))))
    (keyTyped [e])))

(let [window (JFrame. "You Can't Bump The Walls")
      keys-set-atom (atom #{}) ;set of keyboard keys currently being held down by player
      panel (create-panel window-width window-height keys-set-atom)]
  (configure-gui window panel)
  (loop [gamestate (initial-gamestate)
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