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

(defn flush?
  "Returns a boolean value indicating whether or not the specified cards contain a flush"
  [cards]
  (some #(>= (count %) 5) (partition-by first (sort-by first cards))))

(defn rank-to-integer
  "Converts each rank into an integer equivalent"
  [ranks]
  (map #(.indexOf all-ranks %) ranks))

(defn straight?
  "Returns true if the specified cards contain a straight, otherwise nil"
  [cards]
  (some
    #(= % [1 4])
    (map
      (fn [vals] [(first vals) (count vals)])
      (partition-by
        (fn [val] val)
        (map
          (fn [[low,high]] (- high low))
          (partition
            2
            1
            (sort
              (rank-to-integer
                (map last cards)))))))))

(defn -main
  "Deals some cards and evaluates the hand strength"
  [& args]
  (let [hand (take 5 (shuffle(deck)))]
    (println hand)
    hand))
