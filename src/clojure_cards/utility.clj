(ns clojure-cards.core)

(def all-suits [:clubs :diamonds :hearts :spades])
(def all-ranks [:2 :3 :4 :5 :6 :7 :8 :9 :10 :jack :queen :king :ace])

(defn rank->integer
  "Converts a rank to an integer value for sorting"
  ([rank] (rank->integer true rank))
  ([aces-high rank]
  (let [position (.indexOf all-ranks rank)
        rank-count (count all-ranks)]
    (if (and (= (+ 1 position) rank-count) (not aces-high))
      (- position rank-count)
      position))))

(defn integer->rank
  "Converts an integer to the rank equivalent"
  [integer]
  (nth all-ranks integer))

(defn ranks->integers
  "Converts each rank into an integer equivalent"
  ([ranks] (ranks->integers false ranks))
  ([aces-high ranks]
   (map #(rank->integer aces-high %) ranks)))

(defn rank-card
  "Returns a vector containing the specified card in the first position and the sortable rank in the second."
  [aces-high card]
  (vector card (rank->integer aces-high (last card))))

(defn rank-cards
  "Returns a sequence of vectors containing the specified cards and their sortable rank."
  [aces-high cards]
  (map #(rank-card aces-high %) cards))

(defn group-and-sort-by-rank
  "Returns the specified cards grouped by rank and sorted by descending group count."
  [cards]
  (->> cards
       (rank-cards true)
       (sort-by last #(compare %2 %1))
       (partition-by last)
       (sort-by count #(compare %2 %1))
       (map #(map first %))))

(defn inc-rank
  "Returns a card having the next highest rank in the same suit"
  [[suit rank]]
  (vector suit (-> rank
                   rank->integer
                   (+ 1)
                   (mod (count all-ranks))
                   integer->rank)))

(defn missing
  "Returns the cards in list1 not present in list2"
  [list1 list2]
  (filter
    (fn [[_ rank1]]
      (not-any?
        (fn [[_ rank2]] (= rank1 rank2))
        list2))
    list1))

(defn sort-cards
  "Returns the cards sorted in descending order of rank"
  [cards]
  (->> cards
       (map #(vector % (rank->integer (last %))))
       (sort (fn [[_ r1] [_ r2]] (compare r2 r1)))
       (map first)))

(defn find-straight-in-n-cards
  "Returns the cards if they make up a straight, otherwise nil"
  [cards]
  (let [successors (map inc-rank cards)
        cards-not-matched (missing cards successors)
        successors-not-matched (missing successors cards)]
    (if (and (= 1 (count successors-not-matched)) (= 1 (count cards-not-matched)))
      (sort-cards cards))))
