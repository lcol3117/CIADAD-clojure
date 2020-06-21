(defn ciadad [data] (do 
  (def cia-dists
    (map data
      (fn [x] (get-CIAD-not-self x data))))
  (def median-dist
    (median cia-dists))
  (map cia-dists
    (fn [x] (< x median-dist))))
(defn get-CIAD-not-self [point,data] (do 
  (def options
    (map data
      (fn [x] (ciad-not-self x point))))
  (def mindist
    (apply min options))
  (def index-min
    (.indexOf options mindist))
  (get options index-min)))
(defn ciad-not-self [a,b] (do 
  (def initial
    (ciad a b))
  (if 
    (= initial 0)
    initial
    ##Inf)))
(defn ciad [a,b] (do
  (def dimrange
    (range 0
      (count a)))
  (def dists
    (map
      (fn [x] (abs (- (nth a x) (nth b x))))))
  (reduce max dists)))
(defn abs [n] (do
  (max
    n (- 0 n))))
