(ns clojure-cards.core-test
  (:require [clojure.test :refer :all]
            [clojure-cards.core :refer :all :as cards]))

(def royal-flush-hand [[:clubs :ace]
                       [:clubs :queen]
                       [:hearts :queen]
                       [:clubs 10]
                       [:clubs :king]
                       [:clubs :jack]
                       [:diamonds :queen]])
(def straight-flush-hand[[:clubs 9]
                         [:clubs :queen]
                         [:hearts :queen]
                         [:clubs 10]
                         [:clubs :king]
                         [:clubs :jack]
                         [:diamonds :queen]])
(def four-of-a-kind-hand[[:clubs 9]
                         [:clubs :queen]
                         [:hearts :queen]
                         [:diamonds 7]
                         [:spades :queen]
                         [:clubs :jack]
                         [:diamonds :queen]])
(def full-house-hand[[:clubs 9]
                     [:clubs :queen]
                     [:hearts :queen]
                     [:diamonds 7]
                     [:spades :jack]
                     [:clubs :jack]
                     [:diamonds :queen]])
(def flush-hand[[:clubs 9]
                [:clubs :queen]
                [:hearts 2]
                [:clubs 7]
                [:spades :jack]
                [:clubs :jack]
                [:clubs 5]])
(def straight-hand[[:clubs 9]
                   [:clubs :queen]
                   [:hearts 6]
                   [:diamonds 7]
                   [:spades 8]
                   [:hearts :jack]
                   [:clubs 5]])
(def ace-high-straight-hand[[:clubs :ace]
                            [:clubs :queen]
                            [:hearts :king]
                            [:diamonds 7]
                            [:spades 10]
                            [:hearts :jack]
                            [:clubs 5]])
(def ace-low-straight-hand[[:clubs 9]
                           [:clubs 7]
                           [:hearts 3]
                           [:diamonds :ace]
                           [:spades 2]
                           [:hearts 4]
                           [:clubs 5]])
(def three-of-a-kind-hand[[:clubs 9]
                          [:clubs :queen]
                          [:hearts :queen]
                          [:diamonds 7]
                          [:spades :jack]
                          [:clubs 6]
                          [:diamonds :queen]])
(def two-pair-hand[[:clubs 9]
                   [:clubs :queen]
                   [:hearts 10]
                   [:diamonds 7]
                   [:spades :jack]
                   [:clubs :jack]
                   [:diamonds :queen]])
(def pair-hand[[:clubs 9]
               [:clubs :queen]
               [:hearts 10]
               [:diamonds 7]
               [:spades 5]
               [:clubs :jack]
               [:diamonds :queen]])
(def high-card-hand[[:clubs 9]
               [:clubs 3]
               [:hearts 10]
               [:diamonds 7]
               [:spades 5]
               [:clubs :jack]
               [:diamonds :queen]])

(deftest deck-has-52-cards
  (testing "A new deck should have 52 cards"
    (is (= 52 (count (cards/deck))))))

(deftest draw-a-card
  (testing "Drawing a card removes it from the deck"
    (is (= 51 (count (last (cards/draw-card (cards/deck) 1))))))
  (testing "Drawing a card returns the card"
    (is (= [:clubs :ace] (first (cards/draw-card (cards/deck) 0))))))

(deftest find-a-pair
  (testing "Returns the cards making up the pair hand with present"
    (let [result (cards/find-pair pair-hand)]
      (is (not (nil? result)))
      (is (= 5 (count result)))
      (is (= [:queen :queen :jack 10 9] (map last result)))))
  (testing "Considers three-of-a-kind to also be a pair"
    (let [result (cards/find-pair three-of-a-kind-hand)]
      (is (= [:queen :queen :queen :jack 9] (map last result)))))
  (testing "Returns nil if no pair is present"
    (let [ result (cards/find-pair high-card-hand)]
    (is (nil? result)))))

(deftest find-a-two-pair
  (testing "Returns the cards making up the pair hand with present"
    (let [result (cards/find-two-pair two-pair-hand)]
      (is (not (nil? result)))
      (is (= 5 (count result)))
      (is (= [:queen :queen :jack :jack 10] (map last result))))))



(deftest find-a-three-of-a-kind
  (testing "Returns the three-of-a-kind hand from the specified cards when one is present"
    (let [result (cards/find-three-of-a-kind three-of-a-kind-hand)]
      (is (not (nil? result)))
      (is (= 5 (count result)))
      (is (= [:queen :queen :queen :jack 9] (map last result)))))
  (testing "Returns nil when three-of-a-kind is not present"
    (let [result (cards/find-three-of-a-kind pair-hand)]
      (is (nil? result)))))

(deftest find-a-full-house
  (testing "Returns the cards making up a full house when present"
    (let [result (cards/find-full-house full-house-hand)]
      (is (not (nil? result)))
      (is (= [:queen :queen :queen :jack :jack] (map last result)))))
  (testing "Returns nil if no full house can be made"
    (let [result (cards/find-full-house two-pair-hand)]
      (is (nil? result)))))

(deftest find-a-four-of-a-kind
  (testing "Returns the correct cards if they contain four of a kind"
    (let [result (cards/find-four-of-a-kind four-of-a-kind-hand)]
      (is (not (nil? result)))
      (is (= [:queen :queen :queen :queen :jack] (map last result)))))
  (testing "Returns nil if four-of-a-kind is not present"
    (let [result (cards/find-four-of-a-kind three-of-a-kind-hand)]
    (is (nil? result)))))

(deftest find-a-flush
  (testing "Extracts the 5 cards making up a flush from a hand containing one"
    (let [result (cards/find-flush flush-hand)]
      (is (= 5 (count result)))
      (is (= #{:clubs} (set (map first result))))))
  (testing "Returns nil if not a flush"
    (let [result (cards/find-flush straight-hand)]
      (is (nil? result)))))

(deftest find-a-straight
  (testing "Correctly identify a straight"
    (let [result (cards/find-straight straight-hand)]
      (is (= [9 8 7 6 5] (map last result)))))
  (testing "Correctly identify a straight with a low ace"
    (let [result (cards/find-straight ace-low-straight-hand)]
      (is (= [5 4 3 2 :ace] (map last result)))))
  (testing "Correctly identify a straight with a high ace"
    (let [result (cards/find-straight ace-high-straight-hand)]
      (is (= [:ace :king :queen :jack 10] (map last result)))))
  (testing "Correctly identify a straight with more than 5 cards"
    (let [result (cards/find-straight [[:clubs 2] [:hearts 3] [:spades 4] [:clubs 5] [:clubs 6] [:spades 7]])]
      (is (= [7 6 5 4 3] (map last result)))))
  (testing "Returns false if no straight is present"
    (let [result (cards/find-straight high-card-hand)]
      (is (nil? result)))))

(deftest find-a-straight-flush
  (testing "Correctly returns the straight flush from a hand that includes one"
    (let [result (cards/find-straight-flush straight-flush-hand)]
      (is (= [:king :queen :jack 10 9] (map last result)))
      (is (= #{:clubs} (set (map first result))))))
  (testing "Returns nil if no straight is pressent"
    (let [result (cards/find-straight-flush flush-hand)]
      (is (nil? result))))
  (testing "Returns nil if no flush is pressent"
    (let [result (cards/find-straight-flush straight-hand)]
      (is (nil? result)))))

(deftest find-a-royal-flush
  (testing "Correctly returns the royal flush from a hand that includes one"
    (let [result (cards/find-royal-flush royal-flush-hand)]
      (is (= #{10 :jack :queen :king :ace} (set (map last result))))
      (is (= #{:clubs} (set (map first result))))))
  (testing "Returns nil if no royal flush is present"
    (let [result (cards/find-royal-flush straight-flush-hand)]
      (is (nil? result)))))

(deftest find-a-high-card
  (testing "Correctly returns the cards in the right order when only a high card is present"
    (let [result (cards/find-high-card high-card-hand)]
      (is (= [:queen :jack 10 9 7] (map #(last %) result))))))

(deftest evaluate-a-hand
  (testing "Correctly identify a royal flush"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds 3] [:spades :king] [:spades :ace] [:hearts 3] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :royal-flush (first result)))
      (is (= [[:spades :ace] [:spades :king] [:spades :queen] [:spades :jack] [:spades 10]] (last result)))))
  (testing "Correctly identify a straight flush"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds 3] [:spades :king] [:spades 9] [:hearts 3] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :straight-flush (first result)))
      (is (= [[:spades :king] [:spades :queen] [:spades :jack] [:spades 10] [:spades 9]] (last result)))))
  (testing "Correctly identify 4 of a kind"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds :jack] [:hearts :jack] [:spades :ace] [:clubs :jack] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :four-of-a-kind (first result)))
      (is (= [[:spades :king] [:spades :queen] [:spades :jack] [:spades 10] [:spades 9]] (last result)))))
  (testing "Correctly identify a full house"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds :jack] [:hearts :jack] [:spades :ace] [:clubs :ace] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :full-house (first result)))
      (is (= [[:spades :jack] [:diamonds :jack] [:hearts :jack] [:spades :ace] [:clubs :ace]] (last result)))))
  (testing "Correctly identify a flush"
    (let [cards [[:spades :jack] [:spades 10] [:spades 4] [:hearts :jack] [:spades :ace] [:clubs :ace] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :flush (first result)))
      (is (= [[:spades :jack] [:spades 10] [:spades 4] [:spades :ace] [:spades :queen]] (last result)))))
  (testing "Correctly identify a straight"
    (let [cards [[:clubs 4] [:hearts 3] [:diamonds 6] [:spades 5] [:spades :ace] [:diamonds 7] [:hearts :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :straight (first result)))
      (is (= [[:diamonds 7] [:diamonds 6] [:spades 5] [:clubs 4] [:hearts 3]] (last result)))))
  (testing "Correctly identify 3 of a kind"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds :jack] [:hearts :jack] [:spades :ace] [:clubs :king] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :three-of-a-kind (first result)))
      (is (= [[:spades :jack] [:diamonds :jack] [:hearts :jack] [:spades :ace] [:clubs :king]] (last result)))))
  (testing "Correctly identify two pair"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds 8] [:hearts :jack] [:spades :ace] [:clubs :ace] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :two-pair (first result)))
      (is (= [[:spades :ace] [:clubs :ace] [:spades :jack] [:hearts :jack] [:spades :queen]] (last result)))))
  (testing "Correctly identify a pair"
    (let [cards [[:spades :jack] [:spades 10] [:diamonds 8] [:hearts :jack] [:spades 4] [:clubs :ace] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :pair (first result)))
      (is (= [[:spades :jack] [:hearts :jack] [:clubs :ace] [:spades :queen] [:spades 10]] (last result)))))
  (testing "Correctly identify a high-card"
    (let [cards [[:spades :9] [:spades 10] [:diamonds 8] [:hearts :jack] [:spades 4] [:clubs :ace] [:spades :queen]]
          result (cards/evaluate-hand cards)]
      (is (= :high-card (first result)))
      (is (= [:ace :queen :jack 10 9] (map last (last result)))))))
