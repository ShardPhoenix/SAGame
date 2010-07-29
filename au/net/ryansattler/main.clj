;TODO:
;- multiple levels, gui (start button etc), high scores
;- graphical effects, challenge modes, sub-goals, points for squares touched etc
;- powerups?
;- sounds/music
;- tests
;- "theseus and minotaur"
;- diff. music after minotaur is activated (or player is in trouble etc)
;- goal: grab all treasures and escape, don't get caught by minotaur
;- time limit after which minotaur goes straight for you?
;- move to clojure 1.2 beta?

;- have to exit at start instead? - TRY THIS
;- distribute treasures biasedly based on distance from start (and exit if applicable)
;- minotaur, when angered (eg pick up treasure), goes there. otherwise follows player if in line of sight, 
;- otherwise goes back to start
;- minotaur always follows player when maximally angry?
;- increase branching factor of maze (maybe remove random walls)
;- minotaur gets angry based on treasures, time, or both

;- use a map of events for eg sound, clear on each loop?

;- display bombs as red square
;- min number of treasures (eg 4) needed to exit?
;- redo all rendering coords as relative in col/row etc
;- make green trail not go "in front of" player (rendering issue - needs delay or something)

;- add 1 random bomb pickup to each level (+ only give one at end)?
;- save high scores

;- have minotaur start moving after ~5 secs regardless

;-Precompile deployed version!

;- bugs: -minotaur can spawn in wall


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

(defstruct player :coord :last-moved :last-coord :millis-per-move :health :bombs :last-bombed)
(defn make-player [coord]
  (struct player coord (current-time) coord initial-player-millis-per-move 1 starting-bombs 0)) 

(defn initial-gamestate [] 
  (let [level (gen-level)
        minotaurpos (find-minotaur level)]
	  {:score 0
     :levelnum 1
     :started false
     :start-time (current-time) 
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

(defn update-bombed [input level [col row] {:keys [bombs last-bombed]}]
 (let [should-bomb? (<= bomb-delay (- (current-time) last-bombed))]
  (if (and should-bomb? (:bomb input) (pos? bombs))
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
       level)))

(defn update-treasure [level coord treasures-gained]
 (let [treasures-touched (count (filter #(and (:treasure %) (in-piece? % coord)) level))]
    (+ treasures-touched treasures-gained)))

(defn is-in-wall? [coord level]
  (pos? (count (filter true? (map #(and (:wall %) (in-piece? % coord)) level)))))

(defn try-move [[col row :as coord] x-direc y-direc level last-moved millis-per-move]
  (let [thetime (current-time)
        new-coord [(+ col x-direc) (+ row y-direc)]]
	  (if (and (not (and (zero? x-direc) (zero? y-direc))) 
             (>= (- thetime last-moved) millis-per-move)
             (not (is-in-wall? new-coord level)))
	     new-coord
	     coord)))

;also start moving if enough time has elapsed, etc
(defn get-minotaur-route [level coord target treasures start-time]
  (cond
    (or (pos? treasures) (> (- (current-time) start-time) 4000)) (get-route level coord target)
    :else [coord]))

(defn update-minotaur [{:keys [route coord last-moved last-coord millis-per-move] :as minotaur} 
                       level 
                       target 
                       {:keys [treasures-gained start-time] :as game}]
  (let [minotaur (assoc minotaur :route (get-minotaur-route level coord target treasures-gained start-time))]
	  (if (> (count route) 1)
	      (let [[next-col next-row] (second route)
	            x-direc (- next-col (first coord))
	            y-direc (- next-row (second coord))
	            new-coord (try-move coord x-direc y-direc level last-moved millis-per-move)]
		          minotaur (assoc minotaur :last-coord (if-not (= new-coord coord) coord last-coord)
	                                     :coord new-coord
		                                   :last-moved (if-not (= new-coord coord) (current-time) last-moved)))
	      minotaur)))

(defn update-health [player minotaur]
  (if (= (:coord minotaur) (:coord player))
    0
    1))

(defn update-player [{:keys [coord last-moved last-coord last-bombed millis-per-move bombs] :as player} 
                     {:keys [x-direc y-direc bomb]}
                     level 
                     minotaur]
 (let [newcoord (try-move coord x-direc y-direc level last-moved millis-per-move)
       should-bomb? (and  bomb 
                         (pos? bombs) 
                         (<= bomb-delay (- (current-time) last-bombed)))]
  (assoc player :last-coord (if-not (= newcoord coord) coord last-coord)
                :coord newcoord
                :last-moved (if-not (= newcoord coord) (current-time) last-moved)
                :health (update-health player minotaur)
                :bombs (if should-bomb? (dec bombs) bombs)
                :last-bombed (if should-bomb? (current-time) last-bombed))))

(defn update-victory [game]
 (let [[col row] (:coord (game :player))
       health (:health (game :player))]
  (cond 
    (or (>= col maze-size) (>= row maze-size) (< col 0) (< row 0)) 1 ;player has escaped
    (<= health 0) -1 ;player has died
    :else 0)))

(defn update [{:keys [level player minotaur treasures-gained] :as game} input frame window]
 (let [coord (player :coord)]
  (assoc game :treasures-gained (update-treasure level coord treasures-gained)
              :level (update-bombed input (update-touched level coord) coord player)
              :player (update-player player input level minotaur)
              :minotaur (update-minotaur minotaur level coord game)
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

(defn new-level [{:keys [minotaur treasures-gained total-treasures levelnum] :as game} 
                 died?]
	(let [gamestate (initial-gamestate)
        millis (:millis-per-move minotaur)]
	  (assoc gamestate :total-treasures (if died? 0 (+ treasures-gained total-treasures))
                     :score (if died? 0 (+ (:score game) (* treasure-score-constant treasures-gained treasures-gained)))
	                   :levelnum (if died? 1 (inc levelnum))
                     :started true
                     :minotaur (assoc minotaur :millis-per-move (if died? initial-minotaur-millis-per-move
                                                                          (* minotaur-speed-up millis))))))

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

(defn pause-wait [keys-set-atom game panel frame]
  (if (:pause (get-input @keys-set-atom))
    (do (render game panel frame) 
        (recur keys-set-atom game panel frame))))

(let [window (JFrame. "You Can't Escape the Minotaur")
      keys-set-atom (atom #{}) ;set of keyboard keys currently being held down by player
      panel (create-panel window-width window-height keys-set-atom)
      game {:started false}]
  (configure-gui window panel)
  (java.lang.Thread/sleep 500) ;need to wait to make sure screen ready to draw on - better way to check?
  (render game panel 0)
  (java.lang.Thread/sleep start-screen-time)
  (loop [gamestate (assoc (initial-gamestate) :started true)
         frame 1]
    (let [start-time (current-time)
          input (get-input @keys-set-atom)
          gamestate (update gamestate input frame window)]
         (if (:paused gamestate)
           (pause-wait keys-set-atom gamestate window frame))
         (render gamestate panel frame)
    (let [render-time (- (current-time) start-time)
          wait-time (max (- min-millis-per-frame render-time) 0)]
      (if debug
        (println (double render-time)))
      (java.lang.Thread/sleep wait-time))
      (cond (pos? (:victory gamestate)) (do
                                         (render gamestate panel frame)
                                         (java.lang.Thread/sleep end-screen-time)
                                         (recur (new-level gamestate false) (inc frame)))
            (neg? (:victory gamestate)) (do
                                         (render gamestate panel frame)
                                         (java.lang.Thread/sleep end-screen-time)
                                         (recur (new-level gamestate true) (inc frame))) 
            :else (recur gamestate (inc frame))))))










