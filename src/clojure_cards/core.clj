(ns clojure-cards.core
  (:gen-class)
  (load "utility"))

(defn new-deck
  "Returns a new unshuffled deck"
  []
  (for [suit all-suits rank all-ranks]
    [suit rank]))

(defn draw-card
  "Returns an array containing two elements: the drawn card and the remaining deck"
  ([deck] (draw-card deck 0))
  ([deck position]
  (let [[head,tail] (split-at position deck)
        drawn-card (first tail)
        remaining-deck (drop 1 tail)]
    [drawn-card (concat head remaining-deck)])))

(defn find-straight
  "Returns the cards making up a straight, if present in the specified cards. Otherwise returns nil."
  ([cards] (find-straight 5 cards))
  ([length-of-hand cards] (or (find-straight true length-of-hand cards) (find-straight false length-of-hand cards)))
  ([aces-high length-of-hand cards]
  (->> cards
       (sortable aces-high) ; add the sortable rank value
       (sort #(compare (last %2) (last %1))) ; sort by descending rank
       append-sequence-identifier
       (partition-by last) ; group them by the sequence id
       (map #(map ffirst %)) ; remove sequence ids and sortable ranks
       (sort #(compare (first %2) (first %1))) ; sort by highest rank in sequence descending
       (filter #(<= length-of-hand (count %))) ; get sequences that are long enough
       first)))

(defn find-n-of-a-kind
  "Returns the cards that make up the specified n-of-a-kind hand, or nil if the hand can't be made"
  [match-count cards]
  (let [grouped-by-rank (group-and-sort-by-rank cards)
        first-group (first grouped-by-rank)
        remaining-cards (apply concat (rest grouped-by-rank))]
    (if (<= match-count (count first-group))
      (concat
        first-group
        (take (- 5 (count first-group)) remaining-cards)
      nil))))

(defn find-pair
  "Returns the cards making up a pair hand, or nil if no such hand can be made from the specified cards."
  [cards]
  (find-n-of-a-kind 2 cards))

(defn pair?
  "Returns true if the specifiec cards contain a pair, false if not"
  [cards]
  (some? (find-pair cards)))

(defn find-two-pair
  "Returns the cards making of a two-pair hand, or nil if no such hand can be made from the specified cards."
  [cards]
  (let [grouped (group-and-sort-by-rank cards)
        first-group (first grouped)
        first-group-count (count first-group)
        second-group (nth grouped 1)
        second-group-count (count second-group)
        remaining-cards (apply concat (rest (rest grouped)))]
    (if (= 4 first-group-count)
      (concat first-group (first second-group))
      (if (and (<= 2 first-group-count) (<= 2 second-group-count))
        (concat
          first-group
          second-group
          (take
            (- 5 (+ first-group-count second-group-count))
            remaining-cards))
        nil))))

(defn two-pair?
  "Returns true if the specifiec cards contain two pair, false if not"
  [cards]
  (some? (find-two-pair cards)))

(defn find-three-of-a-kind
  "Returns the cards that make up a three-of-a-kind hand from the specified cards, or nil if no such hand can be made."
  [cards]
  (find-n-of-a-kind 3 cards))

(defn three-of-a-kind?
  "Returns true if the specified cards contain three of a kind, false if not"
  [cards]
  (some? (find-three-of-a-kind cards)))

;; This can probably be combined with find-two-pair in some way, but I wasn't ready to take that on
(defn find-full-house
  "Returns the cards making up a full house or nil if no such hand can be made from the specified cards."
  [cards]
  (let [grouped (group-and-sort-by-rank cards)
        first-group (first grouped)
        first-group-count (count first-group)
        second-group (nth grouped 1)
        second-group-count (count second-group)]
    (if (and (= 3 first-group-count) (<= 2 second-group-count))
      (concat first-group (take 2 second-group))
      nil)))

(defn find-four-of-a-kind
  "Returns the cards making up a four-of-a-kind hand if present, nil if not"
  [cards]
  (find-n-of-a-kind 4 cards))

(defn four-of-a-kind?
  "Returns true if the specified cards contain three of a kind, false if not"
  [cards]
  (some? (find-four-of-a-kind cards)))

(defn find-flush
  "Returns the cards making up a flush, if present in the specified cards"
  [cards]
  (->> cards
       (sort-by first)
       (partition-by first)
       (filter #(>= (count %) 5))
       (first)))

(defn flush?
  "Returns a boolean value indicating whether or not the specified cards contain a flush"
  [cards]
  (some? (find-flush cards)))

(defn straight?
  "Returns true if the specified cards contain a straight, otherwise false"
  [cards]
  (some? (find-straight cards)))

(defn find-straight-flush
  "Returns the cards making up a straight flush from the specified cards, if a straight flush exists. Otherwise nil."
  [cards]
  (find-straight 5 (find-flush cards)))

(defn straight-flush?
  "Returns true if the specified cards contain a straight flush, otherwise false"
  [cards]
  (some? (find-straight-flush cards)))

(defn find-royal-flush
  "Returns the cards that make up a royal flush from the specified cards, if present. Otherwise nil."
  [cards]
  (let [sf (find-straight-flush cards)]
    (if (= :ace (last (first sf))) sf nil)))

(defn find-high-card
  "Returns the cards sorted by descending rank"
  [cards]
  (->> cards
       (rank-cards true)
       (sort-by last #(compare %2 %1))
       (map first)
       (take 5)))

(def hand-strength-functions [[find-royal-flush :royal-flush]
                              [find-straight-flush :straight-flush]
                              [find-four-of-a-kind :four-of-a-kind]
                              [find-full-house :full-house]
                              [find-flush :flush]
                              [find-straight :straight]
                              [find-three-of-a-kind :three-of-a-kind]
                              [find-two-pair :two-pair]
                              [find-pair :pair]
                              [find-high-card :high-card]])

(defn decorate-fn
  "Executes a function returning nil if the function returns nil, otherwise a vector containing the key associated with the function and the results of the function."
  [[f k] cards]
  (let [result (f cards)]
    (if (nil? result)
      nil
      [k result])))

(defn evaluate-hand
  "Returns the hand's strength and the cards that make up that strength"
  [cards]
  (some #(seq (decorate-fn % cards)) hand-strength-functions))

(defn deal
  "Deals out x number of hands containing y number of cards from the specified deck"
  [hand-count card-count cards]
  (->> cards
       (take (* hand-count card-count))
       (partition hand-count)
       (apply interleave)
       (partition card-count)
       (map #(apply hash-set %))))

(defn sortable-hand
  "Returns a sequence containing the specified evaluate hand and an integer that can be used to compare this hand to other evaluated hands"
  [[strength cards]]
  (let [index (.indexOf (reverse (map last hand-strength-functions)) strength)]
    (vector index strength cards)))

(defn compare-hands
  "Returns compare-like results for two sortable hands"
  [[index1 strength1 cards1] [index2 strength2 cards2]]
  (let [primary-result (compare index1 index2)]
    (if (= 0 primary-result)
      (let [rank1 (into [] (map #(rank->integer true (last %)) cards1))
            rank2 (into [] (map #(rank->integer true (last %)) cards2))]
        (compare rank1 rank2))
      primary-result)))

(defn five-card-draw
  "Deals 2 hands of 5 cards and declares a winner"
  [player-count]
  (println "--------------")
  (println "five card draw")
  (println "--------------")
  (println)
  (let [hands (->> (new-deck)
                   shuffle
                   (deal player-count 5))
        evaluated (->> hands
                       (map evaluate-hand)
                       (sort #(compare-hands %2 %1))
                       (map (fn [[strength cards]]
                              (vector strength (apply hash-set cards)))))
        ranked (map
                 #(let [e (first (filter (fn [e] (= (last e) %)) evaluated))
                        rank (inc (.indexOf evaluated e))
                        strength (first e)]
                    (vector rank strength %))
                 hands)]
    (doseq [[rank strength cards] ranked]
      (printf "place: %s\n"  rank)
      (printf "strength: %s\n" strength)
      (printf "cards: %s\n" cards)
      (println))))

(defn winner
  "Returns the strongest hand from the specified list of hands"
  [& hands]
  (->> hands
       (map evaluate-hand)
       (map sortable-hand)
       (sort-by identity #(compare-hands %2 %1))
       (map rest)
       first))

(defn -main
  "Deals some cards and evaluates the hand strength"
  [& args]
  (five-card-draw 3))
