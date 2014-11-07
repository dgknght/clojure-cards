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
  "Converts each rank into an integer equivalent"
  ([ranks] (rank->integer false ranks))
  ([aces-high ranks]
  (let [mapped (map #(.indexOf all-ranks %) ranks)]
    (if aces-high
      (replace (hash-map 0 13) mapped)
      mapped))))

(defn x-of-a-kind?
  "Returns true if the hand contains at least the specified number of any rank, otherwise nil"
  [cards kind-count]
  (->> cards
       (map last)
       rank->integer
       sort
       (partition-by identity)
       (some #(>= (count %) kind-count))))

(defn pair?
  "Returns true if the specifiec cards contain a pair, nil if not"
  [cards]
  (x-of-a-kind? cards 2))

(defn three-of-a-kind?
  "Returns true if the specified cards contain three of a kind, nil if not"
  [cards]
  (x-of-a-kind? cards 3))

(defn flush?
  "Returns a boolean value indicating whether or not the specified cards contain a flush"
  [cards]
  (some #(>= (count %) 5) (partition-by first (sort-by first cards))))

(defn five-in-sequence?
  "Returns true if the specified cards have 5 ranks in sequence, nil if not"
  [ranks]
  (->> ranks
       sort
       (partition 2 1)
       (map (fn [[low,high]] (- high low)))
       (partition-by identity)
       (map (fn [vals] [(first vals) (count vals)]))
       (some #(and
                 (= (first %) 1)
                 (>= (last %) 4)))))

(defn straight?
  "Returns true if the specified cards contain a straight, otherwise nil"
  [cards]
  (let [ranks (map last cards)
        aces-low (rank->integer ranks)
        aces-high (rank->integer true ranks)]
      (or
        (five-in-sequence? aces-high)
        (five-in-sequence? aces-low))))


(defn -main
  "Deals some cards and evaluates the hand strength"
  [& args]
  (let [hand (take 5 (shuffle(deck)))]
    (println hand)
    hand))
