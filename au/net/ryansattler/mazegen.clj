(ns au.net.ryansattler.mazegen
  (:use au.net.ryansattler.constants))

(defstruct maze-cell :col :row :x :y :wall :visited :touched)
(defn make-maze-cell [col row wall?] 
  (struct maze-cell 
    col 
    row 
    (+ maze-left-margin (* wall-width col)) 
    (+ maze-top-margin (* wall-width row))
    wall?
    false
    false))

;initial maze with checkerboard pattern of wall/not-wall (outside is all wall).
;return is a map from [row col] vectors (eg [2 3]) to the maze-cell struct at that position.
(defn initial-maze []
  (zipmap
    (for [x (range maze-size) y (range maze-size)] [x y])
	  (for [x (range maze-size) y (range maze-size)] 
	    (make-maze-cell x y (or (zero? (rem x 2)) (zero? (rem y 2)))))))

(defn rand-nth
  [coll]
  (nth coll (rand-int (count coll))))

(defn get-unvisited-neighbour-coords [maze coord]
  (let [row (first coord)
        col (last coord)
        topc [(+ row 2) col]
        bottomc [(- row 2) col]
        leftc [row (- col 2)]
        rightc [row (+ 2 col)]
        neighbours [topc bottomc leftc rightc]]
    (filter #(and (maze %) (not (:visited (maze %)))) neighbours)))

;removes the wall between the two coords
(defn remove-wall [maze coord1 coord2]
  (let [wallc (map #(if (= %1 %2) %1 (dec (max %1 %2))) coord1 coord2)
        ex-wall (assoc (maze wallc) :wall false)]
    (assoc maze wallc ex-wall)))
 
(defn gen-level2 [maze stack coord]
  (let [visited (assoc (maze coord) :visited true)
        maze (assoc maze coord visited)
        neighbours (get-unvisited-neighbour-coords maze coord)]
    (if (seq neighbours)
      (let
        [neighbour (rand-nth neighbours)
         stack (conj stack coord)
         maze (remove-wall maze neighbour coord)]
        (recur maze stack neighbour))
        (if (empty? stack)
           maze
          (recur maze (pop stack) (peek stack))))))

(defn gen-level []
  (let [maze (initial-maze)
        bottom-right [(- maze-size 2) (- maze-size 2)]]
    (vals (gen-level2 maze [] bottom-right))))

