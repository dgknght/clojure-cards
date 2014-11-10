(ns clojure-cards.core
  (:gen-class))

(def all-suits [:clubs :diamonds :hearts :spades])
(def all-ranks [:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king])

(defn deck
  "Returns a new unshuffled deck"
  []
  (for [suit all-suits rank all-ranks]
    [suit rank]))

(defn draw-card
  "Returns an array containing two elements: the drawn card and the remaining deck"
  [deck position]
  (let [[head,tail] (split-at position deck)
        drawn-card (first tail)
        remaining-deck (drop 1 tail)]
    [drawn-card (concat head remaining-deck)]))

(defn rank->integer
  "Converts a rank to an integer value for sorting"
  ([rank] (rank->integer false rank))
  ([aces-high rank]
  (let [position (.indexOf all-ranks rank)]
    (if (and (= position 0) aces-high)
      (+ position (count all-ranks))
      position))))

(defn ranks->integers
  "Converts each rank into an integer equivalent"
  ([ranks] (ranks->integers false ranks))
  ([aces-high ranks]
   (map #(rank->integer aces-high %) ranks)))

(defn get-rank-group-counts
  "Returns a sequence of numbers representing the count of matching ranks in the specified cards in descending count order. E.g. [[:clubs 2] [:clubs 3 ] [:hearts 2] [:diamonds 3] [:spades 10]] => (2 2 1); (two 2's, two 3's and 1 10)"
  [cards]
  (->> cards
       (map last)
       ranks->integers
       sort
       (partition-by identity)
       (map count)
       (sort #(compare %2 %1))))

(defn get-suit-group-counts
  "Returns a sequence of numbers representing the count of matching suits in the specified cards in descending count order. E.g. [[:clubs 5] [:hearts 9] [:spades 5] [:clubs :ace] [:clubs :queen]] => (3 1 1); (three clubs, one heart, one spade)"
  [cards]
  (->> cards
       (map first)
       sort
       (partition-by identity)
       (map count)
       (sort #(compare %2 %1))))

(defn get-sequence-group-counts
  "Returns a sequence of numbers representing the count of cards in sequence by rank in descending order. E.g. [[:clubs 3] [:hearts 2] [:spades :king] [:hearts :queen] [:hearts 4]] => (3 2); ((2 3 4) (queen king))"
  [aces-high cards]
  (->> cards
       (map last)
       (ranks->integers aces-high)
       sort
       (partition 2 1)
       (map (fn [[low high]] (- high low)))
       (partition-by identity)
       (filter #(= 1 (first %)))
       (map count)
       (sort #(compare %2 %1))))

(defn x-of-a-kind?
  "Returns true if the hand contains at least the specified number of any rank, otherwise false"
  ([cards kind-count] (x-of-a-kind? cards kind-count 1))
  ([cards kind-count group-count]
  (let [rank-counts (get-rank-group-counts cards)
        group-counts (take group-count rank-counts)]
    (every? #(>= % kind-count) group-counts))))

(defn pair?
  "Returns true if the specifiec cards contain a pair, false if not"
  [cards]
  (x-of-a-kind? cards 2))

(defn two-pair?
  "Returns true if the specifiec cards contain two pair, false if not"
  [cards]
  (x-of-a-kind? cards 2 2))

(defn three-of-a-kind?
  "Returns true if the specified cards contain three of a kind, false if not"
  [cards]
  (x-of-a-kind? cards 3))

(defn four-of-a-kind?
  "Returns true if the specified cards contain three of a kind, false if not"
  [cards]
  (x-of-a-kind? cards 4))

(defn find-flush
  "Returns the cards making up a flush, if present in the specified cards"
  [cards]
  (->> cards
       (sort-by #(first %))
       (partition-by #(first %))
       (filter #(>= (count %) 5))
       (first)))

(defn flush?
  "Returns a boolean value indicating whether or not the specified cards contain a flush"
  [cards]
  (some? (find-flush cards)))

(defn find-straight-in-ranked-cards
  "Returns the cards making up a straight, if present in the specified cards. Otherwise returns nil."
  [cards]
  (->> cards
       (sort-by #(last %) #(compare %2 %1))
       (#(concat % [[[nil nil] nil]]))
       (partition 2 1)
       (map (let [seq-num (atom 0)]
              (fn test-fn [[[high-card high-rank] [low-card low-rank]]]
                (let [delta (if (nil? low-rank) -1 (- high-rank low-rank))
                      result (vector high-card @seq-num)]
                  (if (< 1 delta) (swap! seq-num inc))
                  result))))
       (partition-by #(last %))
       (sort-by count #(compare %2 %1))
       (filter #(>= (count %) 5))
       first
       (map first)
       seq))

(defn rank-card
  "Returns a vector containing the specified card in the first position and the sortable rank in the second."
  [aces-high card]
  (vector card (rank->integer aces-high (last card))))

(defn rank-cards
  "Returns a sequence of vectors containing the specified cards and their sortable rank."
  [aces-high cards]
  (map #(rank-card aces-high %) cards))

(defn find-straight
  "Returns the cards making up a straight, if present in the specified cards. Otherwise returns nil."
  [cards]
  (let [aces-high (rank-cards true cards)
        aces-low (rank-cards false cards)]
    (or
      (find-straight-in-ranked-cards aces-high)
      (find-straight-in-ranked-cards aces-low))))

(defn straight?
  "Returns true if the specified cards contain a straight, otherwise false"
  [cards]
  (some? (find-straight cards)))

(defn straight-flush?
  "Returns true if the specified cards contain a straight flush, otherwise false"
  [cards]
  false)

(defn -main
  "Deals some cards and evaluates the hand strength"
  [& args]
  (let [hand (take 5 (shuffle(deck)))]
    (println hand)
    hand))
