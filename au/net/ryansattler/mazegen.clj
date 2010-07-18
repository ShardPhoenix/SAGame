(ns au.net.ryansattler.mazegen
  (:use au.net.ryansattler.constants))

(defstruct maze-cell :col :row :x :y :wall :visited :touched :treasure :minotaur-start :exit)
(defn make-maze-cell [col row wall? exit?] 
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

(defn is-exit? [col row]
  (and (= col (- maze-size 2)) (= row (- maze-size 1))))

(defn starts-as-wall? [col row]
  (and (not (is-exit? col row)) 
       (or (zero? (rem col 2)) (zero? (rem row 2)))))

;initial maze with checkerboard pattern of wall/not-wall (outside is all wall).
;return is a map from [row col] vectors (eg [2 3]) to the maze-cell struct at that position.
(def initial-maze
  (zipmap
    (for [col (range maze-size) row (range maze-size)] [col row])
	  (for [col (range maze-size) row (range maze-size)] 
	    (make-maze-cell col 
                      row 
                      (starts-as-wall? col row)
                      (is-exit? col row)))))

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

(defn get-unvisited-neighbour-coords [maze [col row]]
  (let [topc    [(+ col 2) row]
        bottomc [(- col 2) row]
        leftc   [col (- row 2)]
        rightc  [col (+ 2 row)]
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

(defn set-random-flags [n flagtype filter-fn maze]
    (let [spaces (filter-fn maze)
          relevant-spaces (take n (shuffle spaces))]
       (for [x maze] 
         (if (some #{x} relevant-spaces) 
           (assoc x flagtype true)
           x))))

(defn has-3-walls [mazepiece maze]
  (let [col (:col mazepiece)
        row (:row mazepiece)
        neighbours [[(inc col) row] [(dec col) row] [col (inc row)] [col (dec row)]]]
    (>= (count (filter #(and (some #{[(:col %) (:row %)]} neighbours) (:wall %)) maze)) 3)))

(defn treasure-spaces [maze]
  (filter #(and (not (:wall %))
                (has-3-walls % maze)) maze))

;return just the values (actual maze-cells) for now. Might use whole map later if needed.
(defn gen-level []
  (let [maze initial-maze
        bottom-right [(- maze-size 2) (- maze-size 2)]]
    (set-random-flags 1 :minotaur-start
            ;make sure minotaur starts in bottom right of maze
            (fn [spaces] (filter #(and (> (:col %) (/ maze-size 2)) (> (:row %) (/ maze-size 2))) spaces))
      (set-random-flags num-treasures :treasure (fn [x] (treasure-spaces x))
        (vals (gen-level2 maze [] bottom-right))))))

