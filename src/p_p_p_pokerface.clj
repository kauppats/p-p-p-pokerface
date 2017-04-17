(ns p-p-p-pokerface)

(def replacements
  {\T 10 \J 11 \Q 12 \K 13 \A 14}
)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r)
    )
  )
)

(defn suit [card]
  (str (get card 1))
)

(defn max-of-kind [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
       ]
    (apply max (vals freqs))
  )
)

(defn pair? [hand]
  (== (max-of-kind hand) 2)  
)

(defn three-of-a-kind? [hand]
  (== (max-of-kind hand) 3)
)

(defn four-of-a-kind? [hand]
  (== (max-of-kind hand) 4)
)

(defn flush? [hand]
  (and
    (== (max-of-kind hand) 1)
    (== (count (set (map suit hand))) 1)
  )
)

(defn rank-counts [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        counts (vals freqs)
       ]
    (vec (sort > counts))
  )
)

;(defn full-house? [hand]
;  (let [counts (rank-counts hand)]
;    (and
;      (== (counts 0) 3)
;      (== (counts 1) 2)
;    )
;  )
;)

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        counts (vals freqs)
        sorted-counts (sort counts)
       ]
    (= sorted-counts (range 2 4)) ; [2 3]
  )
)

(defn two-pairs? [hand]
  (let [counts (rank-counts hand)]
    (and
      (== (counts 0) 2)
      (== (counts 1) 2)
    )
  )
)

(defn _straight? [ranks]
  (let [sorted-ranks (sort ranks)
        min-rank (apply min sorted-ranks)
       ]
    (= sorted-ranks (range min-rank (+ min-rank 5)))
  )
)

(defn straight? [hand]
  (let [high-ace (map rank hand)
        low-ace (replace {14 1} high-ace)
       ]
    (or 
      (_straight? high-ace)
      (_straight? low-ace)
    )
  )
)

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)
  )
)

(defn high-card? [hand]
  true
)

(def value-checkers
  #{
     [high-card? 0]
     [pair? 1]
     [two-pairs? 2]
     [three-of-a-kind? 3]
     [straight? 4]
     [flush? 5]
     [full-house? 6]
     [four-of-a-kind? 7]
     [straight-flush? 8]
   }
)

(defn value [hand]
  (let [func-filter (fn [f] ((first f) hand))
        applied-funcs (filter func-filter value-checkers)
        values (map second applied-funcs)
       ]
    (apply max values)
  )
)

