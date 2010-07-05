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

(defn initial-maze []
  (for [x (range maze-size) y (range maze-size)] 
    (make-maze-cell x y (or (zero? (rem x 2)) (zero? (rem y 2))))))

(defn gen-level2 [maze]
  (for [cell maze]
    (assoc cell :wall (> (Math/random) 0.5))))

(defn gen-level []
  (gen-level2 (initial-maze)))

