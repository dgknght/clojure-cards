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
                   [:hearts 3]
                   [:diamonds 7]
                   [:spades :jack]
                   [:clubs :jack]
                   [:diamonds :queen]])
(def pair-hand[[:clubs 9]
               [:clubs :queen]
               [:hearts 3]
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
    (is (= 52 (count (cards/new-deck))))))

(deftest draw-a-card
  (testing "Drawing a card removes it from the deck"
    (is (= 51 (count (last (cards/draw-card (cards/new-deck) 1))))))
  (testing "Drawing a card returns the card"
    (is (= [:clubs :ace] (first (cards/draw-card (cards/new-deck) 0))))))

(deftest find-a-pair
  (testing "Returns the cards making up the pair hand with present"
    (let [result (cards/find-pair pair-hand)]
      (is (not (nil? result)))
      (is (= [:queen :queen :jack 9 7] (map last result)))))
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
      (is (= [:queen :queen :jack :jack 9] (map last result))))))



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
      (is (nil? result))))
  (testing "Is not confused by duplicate ranks"
    (let [cards [[:clubs 3] [:hearts 4] [:clubs 4] [:diamonds 5] [:spades 6]]
          result (cards/find-straight cards)]
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
    (let [[strength _] (cards/evaluate-hand royal-flush-hand)]
      (is (= :royal-flush strength))))
  (testing "Correctly identify a straight flush"
    (let [[strength _] (cards/evaluate-hand straight-flush-hand)]
      (is (= :straight-flush strength))))
  (testing "Correctly identify 4 of a kind"
    (let [[strength _] (cards/evaluate-hand four-of-a-kind-hand)]
      (is (= :four-of-a-kind strength))))
  (testing "Correctly identify a full house"
    (let [[strength _] (cards/evaluate-hand full-house-hand)]
      (is (= :full-house strength))))
  (testing "Correctly identify a flush"
    (let [[strength _] (cards/evaluate-hand flush-hand)]
      (is (= :flush strength))))
  (testing "Correctly identify a straight"
    (let [[strength _] (cards/evaluate-hand straight-hand)]
      (is (= :straight strength))))
  (testing "Correctly identify 3 of a kind"
    (let [[strength _] (cards/evaluate-hand three-of-a-kind-hand)]
      (is (= :three-of-a-kind strength))))
  (testing "Correctly identify two pair"
    (let [[strength _] (cards/evaluate-hand two-pair-hand)]
      (is (= :two-pair strength))))
  (testing "Correctly identify a pair"
    (let [[strength _] (cards/evaluate-hand pair-hand)]
      (is (= :pair strength))))
  (testing "Correctly identify a high-card"
    (let [[strength _] (cards/evaluate-hand high-card-hand)]
      (is (= :high-card strength)))))

(deftest deal-a-hand
  (testing "Gives the right number of cards to a single hand"
    (let [result (cards/deal 1 5 (new-deck))]
      (is (= 1 (count result)))
      (let [[hand] result]
        (is (= [[:clubs :ace] [:clubs 2] [:clubs 3] [:clubs 4] [:clubs 5]] hand) "The single hand should have the right cards"))))
  (testing "Gives the right number of cards in the right order to two different hands"
    (let [result (cards/deal 2 5 (new-deck))]
      (is (= 2 (count result)))
      (let [[hand1 hand2] result]
        (is (= [[:clubs :ace] [:clubs 3] [:clubs 5] [:clubs 7] [:clubs 9]] hand1) "The first hand should have the right cards")
        (is (= [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:clubs 10]] hand2) "The second hand should have the right cards")))))

(deftest find-a-winner
  (testing "A pair beats a high card"
    (is (= :pair (first (cards/winner high-card-hand pair-hand)))))
  (testing "two-pair beats a pair"
    (is (= :two-pair (first (cards/winner pair-hand two-pair-hand)))))
  (testing "three-of-a-kind beats two-pair"
    (is (= :three-of-a-kind (first (cards/winner two-pair-hand three-of-a-kind-hand)))))
  (testing "straight beats three-of-a-kind"
    (is (= :straight (first (cards/winner three-of-a-kind-hand straight-hand)))))
  (testing "flush beats straight"
    (is (= :flush (first (cards/winner straight-hand flush-hand)))))
  (testing "full house beats flush"
    (is (= :full-house (first (cards/winner flush-hand full-house-hand)))))
  (testing "four-of-a-kind beats full house"
    (is (= :four-of-a-kind (first (cards/winner full-house-hand four-of-a-kind-hand)))))
  (testing "straight-flush beats four-of-a-kind"
    (is (= :straight-flush (first (cards/winner four-of-a-kind-hand straight-flush-hand)))))
  (testing "royal flush beats straight flush"
    (is (= :royal-flush (first (cards/winner straight-flush-hand royal-flush-hand))))))

(deftest compare-same-strength-hands
  (testing "A higher card beats a high card"
    (let [other-hand [[:spades 5] [:diamonds 8] [:clubs 3] [:spades :ace] [:hearts 6]]
          [_ winning-cards] (cards/winner high-card-hand other-hand)
          highest-card (first winning-cards)]
      (is (= [:spades :ace] highest-card))))
  (testing "A higher pair beats a pair"
    (let [other-hand [[:spades 5] [:diamonds 8] [:clubs 3] [:spades :ace] [:hearts :ace]]
          [_ winning-cards] (cards/winner pair-hand other-hand)
          [[_ highest-rank]] winning-cards]
      (is (= :ace highest-rank))))
  (testing "A higher pair of two beats another two pair"
    (let [other-hand [[:spades 5] [:diamonds 5] [:clubs 3] [:spades :ace] [:hearts :ace]]
          [_ winning-cards] (cards/winner two-pair-hand other-hand)
          [[_ highest-rank]] winning-cards]
      (is (= :ace highest-rank))))
  (testing "A higher second pair of two beats another two pair with a matching high pair"
    (let [other-hand [[:spades 5] [:diamonds 5] [:clubs 3] [:spades :queen] [:hearts :queen]]
          [_ winning-cards] (cards/winner other-hand two-pair-hand)
          [_ _ [_ highest-rank]] winning-cards]
      (is (= :jack highest-rank)))))
