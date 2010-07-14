(ns au.net.ryansattler.mazegen
  (:use au.net.ryansattler.constants))

(defstruct maze-cell :col :row :x :y :wall :visited :touched :treasure :minotaur-start)
(defn make-maze-cell [col row wall?] 
  (struct maze-cell 
    col 
    row 
    (+ maze-left-margin (* wall-width col)) 
    (+ maze-top-margin (* wall-width row))
    wall?
    false
    false
    false
    false))


;initial maze with checkerboard pattern of wall/not-wall (outside is all wall).
;return is a map from [row col] vectors (eg [2 3]) to the maze-cell struct at that position.
(def initial-maze
  (zipmap
    (for [x (range maze-size) y (range maze-size)] [x y])
	  (for [x (range maze-size) y (range maze-size)] 
	    (make-maze-cell x y (or (zero? (rem x 2)) (zero? (rem y 2)))))))

;source copied from clojure 1.2 as this is still on 1.1
(defn rand-nth [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  "Return a random permutation of coll"
  {:added "1.2"}
  [coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al)
    (clojure.lang.RT/vector (.toArray al))))

(defn get-unvisited-neighbour-coords [maze coord]
  (let [row     (first coord)
        col     (last coord)
        topc    [(+ row 2) col]
        bottomc [(- row 2) col]
        leftc   [row (- col 2)]
        rightc  [row (+ 2 col)]
        neighbours [topc bottomc leftc rightc]]
    (filter #(and (maze %) (not (:visited (maze %)))) neighbours)))

;removes the wall between the two coords
(defn remove-wall [maze coord1 coord2]
  (let [wallc (map #(if (= %1 %2) %1 (dec (max %1 %2))) coord1 coord2)
        ex-wall (assoc (maze wallc) :wall false)]
    (assoc maze wallc ex-wall)))
 
(defn gen-level2 [maze stack coord]
  (let [maze (assoc maze coord (assoc (maze coord) :visited true))
        neighbours (get-unvisited-neighbour-coords maze coord)]
    (if (seq neighbours)
      (let
        [neighbour (rand-nth neighbours)
         maze (remove-wall maze neighbour coord)]
        (recur maze (conj stack coord) neighbour))
      (if (empty? stack)
         maze
        (recur maze (pop stack) (peek stack))))))

(defn spaces [mazepieces]
  (filter #(not (or (zero? (rem (:col %) 2)) (zero? (rem (:row %) 2)))) mazepieces))

(defn set-random-flags [n flagtype maze]
    (let [spaces (spaces maze)
          relevant-spaces (take n (shuffle spaces))]
       (for [x maze] 
         (if (some #{x} relevant-spaces) 
           (assoc x flagtype true)
           x))))

;return just the values (actual maze-cells) for now. Might use whole map later if needed.
(defn gen-level []
  (let [maze initial-maze
        bottom-right [(- maze-size 2) (- maze-size 2)]]
    (set-random-flags 1 :minotaur-start 
      (set-random-flags num-treasures :treasure 
        (vals (gen-level2 maze [] bottom-right))))))

