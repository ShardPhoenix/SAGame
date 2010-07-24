(ns au.net.ryansattler.pathfinding)

(defn get-neighbours [maze [col row]]
  (let [neighbours [;diagonals commented out for now - smoother look with only orthogonal movement
                    ;(maze [(inc col) (inc row)])
                    ;(maze [(inc col) (dec row)])
                    ;(maze [(dec col) (inc row)])
                    ;(maze [(dec col) (dec row)])
                    (maze [(inc col) row])
                    (maze [col (inc row)])
                    (maze [col (dec row)])
                    (maze [(dec col) row])]]
    (for [cell (filter #(and (not (nil? %)) (not (:wall %))) neighbours)]
        [(:col cell) (:row cell)])))

(defn abs [x]
  (if (< x 0) (- x) x)) 

(defn manhattan-dist [[a b] [x y]]
  (+ (abs (- a x)) (abs (- b y))))

(defn lowest-f2 [open f best-f best-so-far]
  (let [current (first open)
         currentf (f current)]
    (if (not (empty? open))
      (if (< currentf best-f)
        (recur (disj open current) f currentf current)
        (recur (disj open current) f best-f best-so-far))
      best-so-far)))

(defn lowest-f [open f]
  (lowest-f2 open f (f (first open)) (first open))) 

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
          good-neighbours (filter #(or (not (open %)) (< tentative-g (g %))) neighbours)]
      (if (seq good-neighbours)
         (let 
            [open (apply conj open good-neighbours)
            came-from (apply assoc came-from (concat (interpose x good-neighbours) [x]))
            g (apply assoc g (concat (interpose tentative-g good-neighbours) [tentative-g]))
            h (apply assoc h (interleave good-neighbours (map #(manhattan-dist % end) good-neighbours)))
            f (apply assoc f (interleave good-neighbours (map #(+ (g %) (h %)) good-neighbours)))]
          (recur maze end closed open g h f came-from))
        (recur maze end closed open g h f came-from)))))))

(defn get-route [level start end]
 (let [maze (into {} (for [cell level] [[(:col cell) (:row cell)] cell]))] 
  (get-route2 maze end #{} #{start} {start 0} 
        {start (manhattan-dist start end)} {start (manhattan-dist start end)} {})))


