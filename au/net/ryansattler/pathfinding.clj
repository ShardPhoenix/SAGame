(ns au.net.ryansattler.pathfinding)

(defn get-neighbours [maze [col row]]
  (let [neighbours [(maze [(inc col) row])
                    (maze [col (inc row)])
                    (maze [col (dec row)])
                    (maze [(dec col) row])]]
    (for [cell (filter #(and (not (nil? %)) (not (:wall %))) neighbours)]
        [(:col cell) (:row cell)])))

(defn abs [x]
  (if (< x 0) (- x) x)) 

(defn manhattan-dist [[a b] [x y]]
  (+ (abs (- a x)) (abs (- b y))))

;could be much more concise with reduce but it's far too slow...
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

(defn a-star [maze end closed open g h f came-from]
  (if (empty? open) 
    nil
	  (let [current (lowest-f open f)]
	    (if (= current end)
	      (reconstruct-path came-from end)
	    (let [open (disj open current)
	          closed (conj closed current)
	          neighbours (filter #(not (closed %)) (get-neighbours maze current))
	          tentative-g (+ (g current) 1)
	          good-neighbours (filter #(or (not (open %)) (< tentative-g (g %))) neighbours)]
	      (if (seq good-neighbours)
          (let 
	          [open (apply conj open good-neighbours)
	           came-from (merge came-from (into {} (for [neighbour good-neighbours] [neighbour current]))) 
	           g (merge g (into {} (for [neighbour good-neighbours] [neighbour tentative-g])))
	           h (merge h (into {} (for [neighbour good-neighbours] [neighbour (manhattan-dist neighbour end)])))
	           f (merge f (into {} (for [neighbour good-neighbours] [neighbour (+ (g neighbour) (h neighbour))])))]
            (recur maze end closed open g h f came-from))
	        (recur maze end closed open g h f came-from)))))))

(defn get-route [level start end]
 (let [maze (into {} (for [cell level] [[(:col cell) (:row cell)] cell]))] 
  (a-star maze end #{} #{start} {start 0} 
    {start (manhattan-dist start end)} {start (manhattan-dist start end)} {})))


