(defn ciadad [data] (do 
  (def cia-dists
    (map data
      (fn [x] (getCIAD-not-self x data))))
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
