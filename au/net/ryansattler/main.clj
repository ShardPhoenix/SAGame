;TODO:
;- add time limit and win condition, lose condition
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- powerups?
;- hardcore modes: 1 hit/life, narrower corridors, low time-limit
;- sounds/music
;- start, pause buttons
;- distributable package and/or applet!!!!!!!!!!
;- tests
;- smooth animation?
;- "theseus and minotaur", treasures, no move through walls, touching walls (treasures, etc) alerts minotaur?
;- diff. music after minotaur is activated
;- goal: grab all treasures and escape, don't get caught by minotaur
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

(defn find-minotaur [level]
  (let [minotaur-start (first (filter #(true? (:minotaur-start %)) level))]
    [(:col minotaur-start) (:row minotaur-start)]))

(defn initial-gamestate [] 
  (let [level (gen-level)
        minotaurpos (find-minotaur level)]
	  {:score 0
     :levelnum 1
	   :level level
     :maze (zipmap (for [cell level] [(:col cell) (:row cell)]) level) 
	   :playerpos [1 1]
	   :minotaurpos minotaurpos
     :treasures-gained 0
     :route [minotaurpos]}))

(defn get-neighbours [maze [col row]]
  (let [neighbours [(maze [(inc col) row])
                    (maze [(inc col) (inc row)])
                    (maze [col (inc row)])
                    (maze [(dec col) (inc row)])
                    (maze [(dec col) (dec row)])
                    (maze [col (dec row)])
                    (maze [(dec col) row])
                    (maze [(inc col) (dec row)])]]
    (for [cell (filter #(and (not (nil? %)) (not (:wall %))) neighbours)]
        [(:col cell) (:row cell)])))

(defn abs [x]
  (if (< x 0) (- x) x)) 

(defn manhattan-dist [[a b] [x y]]
  (+ (abs (- a x)) (abs (- b y))))

(defn lowest-f [open f]
  (last (first 
    (sort #(- (first %1) (first %2)) 
      (for [cell open] [(f cell) cell])))))

(defn reconstruct-path [came-from current]
  (if (came-from current)
    (concat (reconstruct-path came-from (came-from current)) [current])
    [current])) 

(defn get-route2 [maze end closed open g h f came-from]
  (if (empty? open) nil
  (let [x (lowest-f open f)]
    (if (= x end)
      (reconstruct-path came-from end)
    (let [open (disj open x)
          closed (conj closed x)
          neighbours (filter #(not (closed %)) (get-neighbours maze x))
          tentative-g (+ (g x) 1)
          good-neighbours (filter #(not (nil? %)) 
                (for [y neighbours] (if (or (not (open y)) (< tentative-g (g y))) y)))]
      (if (seq good-neighbours)
         (let 
	          [open (apply conj open good-neighbours)
	          came-from (apply assoc came-from (concat (interpose x good-neighbours) [x]))
	          g (apply assoc g (concat (interpose tentative-g good-neighbours) [tentative-g]))
	          h (apply assoc h (interleave good-neighbours (for [n good-neighbours] (manhattan-dist n end))))
	          f (apply assoc f (interleave good-neighbours (for [n good-neighbours] (+ (g n) (h n)))))]
          (recur maze end closed open g h f came-from))
        (recur maze end closed open g h f came-from)))))))

(defn get-route [maze start end]
  (get-route2 maze end #{} #{start} {start 0} 
        {start (manhattan-dist start end)} {start (manhattan-dist start end)} {}))

(defn current-time []
  (/ (java.lang.System/nanoTime) 1000000))

(def last-moved (atom (current-time)))

(defn in-piece? [piece [col row]]
    (and (= (:col piece) col)
         (= (:row piece) row))) 

(defn update-touched [level coord]
  (map #(if (in-piece? % coord)
           (assoc % 
              :touched true
              :treasure false)
           %) 
    level))

(defn update-treasure [level coord treasures-gained]
 (let [treasures-touched (count (filter #(and (:treasure %) (in-piece? % coord)) level))]
    (+ treasures-touched treasures-gained)))

(defn is-in-wall? [coord level]
  (pos? (count (filter true? (map #(and (:wall %) (in-piece? % coord)) level)))))

;add wall-bumping somehow - multiple returns?
;put time in game map rather than use atom
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

(defn update-minotaur [route [col row] level]
  (if (< (count route) 2)
      [col row] 
  (let [nextmove (second route)
        x-direc (- (first nextmove) col)
        y-direc (- (second nextmove) row )]
       (println "min x y" x-direc y-direc) 
      ;can't use this properly right now - need to fix timer so doesn't clash with player 
      (try-move [col row] x-direc y-direc level))))

(defn update [game input frame window]
 (let [coord (game :playerpos)
       level (game :level)]
  (assoc game :treasures-gained (update-treasure level coord (game :treasures-gained))
              :level (update-touched level coord)
              :playerpos (try-move coord (input :x-direc) (input :y-direc) level)
              :route (get-route (game :maze) (game :minotaurpos) coord)
              :minotaurpos (update-minotaur (:route game) (:minotaurpos game) level)
)))

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