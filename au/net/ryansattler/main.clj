;TODO:
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- powerups?
;- sounds/music
;- pause key
;- tests
;- "theseus and minotaur"
;- diff. music after minotaur is activated (or player is in trouble etc)
;- goal: grab all treasures and escape, don't get caught by minotaur
;- time limit after which minotaur goes straight for you?
;- move to clojure 1.2 beta?
;- instructions on left side of screen

;- have to exit at start instead? - TRY THIS
;- distrbute treasures biasedly based on distance from start (and exit if applicable)
;- minotaur, when angered (eg pick up treasure), goes there. otherwise follows player if in line of sight, 
;- otherwise goes back to start
;- minotaur always follows player when maximally angry?
;- increase branching factor of maze (maybe remove random walls)
;- minotaur gets angry based on treasures, time, or both
;- max minotaur speed gets faster and/or speed increases faster as levels go on (hence the "can't escape")

;- use a map of events for eg sound, clear on each loop?

;- dynamite - smash through walls but angers minotaur? - maybe stuns minotaur first?
;- need to wait between bombs rather than using all up with one press, or use keyTyped?
;- min number of treasures (eg 4) needed to exit?
;- redo all rendering coords as relative in col/row etc
;- make green trail not go "in front of" player (rendering issue - needs delay or something)

;- bugs: -minotaur can spawn in wall and get stuck (maybe fixed?)


(ns au.net.ryansattler.main
  (:import
    (java.awt Dimension)
    (javax.swing JFrame JPanel)
    (java.awt.event KeyListener)
    (java.awt.event KeyEvent))
  (:use au.net.ryansattler.constants)
  (:use [au.net.ryansattler.graphics  :only (render configure-gui current-time)])
  (:use [au.net.ryansattler.mazegen :only (gen-level)])
  (:use [au.net.ryansattler.pathfinding :only (get-route)])
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE VK_SHIFT VK_CONTROL VK_P)

(if debug 
  (set! *warn-on-reflection* true))

(defn find-minotaur [level]
  (let [minotaur-start (first (filter #(true? (:minotaur-start %)) level))]
    [(:col minotaur-start) (:row minotaur-start)]))

(defstruct minotaur :coord :route :last-moved :last-coord :millis-per-move :anger)
(defn make-minotaur [coord]
  (struct minotaur coord [coord] (current-time) coord initial-minotaur-millis-per-move 0)) 

(defstruct player :coord :last-moved :last-coord :millis-per-move :health :bombs)
(defn make-player [coord]
  (struct player coord (current-time) coord initial-player-millis-per-move 1 2)) 

(defn initial-gamestate [] 
  (let [level (gen-level)
        minotaurpos (find-minotaur level)]
	  {:score 0
     :levelnum 1
     :started false
     :victory 0
	   :level level
	   :minotaur (make-minotaur minotaurpos)
     :player (make-player [1 1]) 
     :treasures-gained 0
     :total-treasures 0
     :paused false}))

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

(defn edge-wall? [wall]
  (or
	 (= (:col wall) (dec maze-size))
	 (= (:col wall) 0)
	 (= (:row wall) (dec maze-size))
	 (= (:row wall) 0)))

(defn update-bombed [input level [col row] bombs]
  (if (and (:bomb input) (pos? bombs))
	      (let [neighbours [[(inc col) (inc row)]
	                       [(inc col) (dec row)]
	                       [(dec col) (inc row)]
	                       [(dec col) (dec row)]
	                       [(inc col) row]
	                       [col (inc row)]
	                       [col (dec row)]
	                       [(dec col) row]]]
	          (map #(if (and (some #{[(:col %) (:row %)]} neighbours) (not (edge-wall? %)))
	                     (assoc % :wall false)
	                      %)
	                level))
       level))

(defn update-treasure [level coord treasures-gained]
 (let [treasures-touched (count (filter #(and (:treasure %) (in-piece? % coord)) level))]
    (+ treasures-touched treasures-gained)))

(defn is-in-wall? [coord level]
  (pos? (count (filter true? (map #(and (:wall %) (in-piece? % coord)) level)))))

(defn try-move [[col row] x-direc y-direc level last-moved millis-per-move]
  (let [thetime (current-time)
        newcoord [(+ col x-direc) (+ row y-direc)]]
	  (if (and (not (and (zero? x-direc) (zero? y-direc))) 
             (>= (- thetime last-moved) millis-per-move)
             (not (is-in-wall? newcoord level)))
	     newcoord
	     [col row])))

(defn update-minotaur [minotaur level target]
  (let [route (:route minotaur)
        [col row] (:coord minotaur)
        moved (:last-moved minotaur)
        minotaur (assoc minotaur :route (get-route level [col row] target))]
  (if (> (count (:route minotaur)) 1)
      (let [nextmove (second (:route minotaur))
            x-direc (- (first nextmove) col)
            y-direc (- (second nextmove) row)
            newcoord (try-move [col row] x-direc y-direc level moved (:millis-per-move minotaur))]
	      minotaur (assoc minotaur :last-coord (if-not (= newcoord [col row]) [col row] (minotaur :last-coord))
                                 :coord newcoord
	                               :last-moved (if-not (= newcoord [col row]) (current-time) moved)))
      minotaur)))

(defn update-health [player minotaur]
  (if (= (:coord minotaur) (:coord player))
    0
    1))

(defn update-player [player input level minotaur]
 (let [coord (:coord player) 
       newcoord (try-move coord (input :x-direc) (input :y-direc) level (player :last-moved) (:millis-per-move player))
       bombs (:bombs player)]
  (assoc player :last-coord (if-not (= newcoord coord) coord (player :last-coord))
                :coord newcoord
                :last-moved (if-not (= newcoord coord) (current-time) (player :last-moved))
                :health (update-health player minotaur)
                :bombs (if (and (:bomb input) (pos? bombs)) (dec bombs) bombs))))

(defn update-victory [game]
 (let [[col row] (:coord (game :player))
       health (:health (game :player))]
  (cond 
    (or (>= col maze-size) (>= row maze-size) (< col 0) (< row 0)) 1 ;player has escaped
    (<= health 0) -1 ;player has died
    :else 0)))

(defn update [game input frame window]
 (let [level (game :level)
       player (game :player) 
       coord (player :coord)
       minotaur (game :minotaur)]
  (assoc game :treasures-gained (update-treasure level coord (game :treasures-gained))
              :level (update-bombed input (update-touched level coord) coord (:bombs player))
              :player (update-player player input level minotaur)
              :minotaur (update-minotaur minotaur level coord)
              :victory (update-victory game)
              :paused (:pause input))))

(defn get-input [keys-set] 
  (let [left (if (keys-set VK_LEFT) -1 0)
        right (if (keys-set VK_RIGHT) 1 0)
        up (if (keys-set VK_UP) -1 0)
        down (if (keys-set VK_DOWN) 1 0)]
        {:x-direc (+ left right) 
         :y-direc (+ up down)
         :bomb (keys-set VK_CONTROL)
         :pause (keys-set \p)}))

(defn new-level [game died?]
	(let [gamestate (initial-gamestate)]
	  (assoc gamestate :total-treasures (if died? 0 (+ (:treasures-gained game) (:total-treasures game)))
                     :score (if died? 0 (+ (:score game) (* (:treasures-gained game) (:treasures-gained game))))
	                   :levelnum (if died? 1 (inc (:levelnum game)))
                     :started true)))

(defn create-panel [width height key-code-atom]
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. width height))
    (keyPressed [#^KeyEvent e]
      (if-not (= (.getKeyCode e) VK_P) 
        (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom (.getKeyCode e)))))
    (keyReleased [#^KeyEvent e]
      (if-not (= (.getKeyCode e) VK_P)
        (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom (.getKeyCode e)))))
    (keyTyped [#^KeyEvent e]
      (if (= (.getKeyChar e) \p)
        (if-not (@key-code-atom \p)
          (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom (.getKeyChar e)))
          (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom (.getKeyChar e))))))))

(defn pause-wait [keys-set-atom game window frame]
  (if (:pause (get-input @keys-set-atom))
    (do (render game window frame) 
        (recur keys-set-atom game window frame)))) 

(let [window (JFrame. "You Can't Escape the Minotaur")
      keys-set-atom (atom #{}) ;set of keyboard keys currently being held down by player
      panel (create-panel window-width window-height keys-set-atom)
      game {:started false}]
  (configure-gui window panel)
  (java.lang.Thread/sleep 500) ;need to wait to make sure screen ready to draw on - better way to check?
  (render game window 0)
  (java.lang.Thread/sleep start-screen-time)
  (loop [gamestate (assoc (initial-gamestate) :started true)
         frame 1]
    (let [start-time (current-time)
          input (get-input @keys-set-atom)
          gamestate (update gamestate input frame window)]
         (if (:paused gamestate)
           (pause-wait keys-set-atom gamestate window frame))
         (render gamestate window frame)
    (let [render-time (- (current-time) start-time)
          wait-time (max (- min-millis-per-frame render-time) 0)]
      (if debug
        (println (double render-time)))
      (java.lang.Thread/sleep wait-time))
      (cond (pos? (:victory gamestate)) (do
                                         (render gamestate window frame)
                                         (java.lang.Thread/sleep end-screen-time)
                                         (recur (new-level gamestate false) (inc frame)))
            (neg? (:victory gamestate)) (do
                                         (render gamestate window frame)
                                         (java.lang.Thread/sleep end-screen-time)
                                         (recur (new-level gamestate true) (inc frame))) 
            :else (recur gamestate (inc frame))))))










