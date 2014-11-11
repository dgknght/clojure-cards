(ns clojure-cards.core-test
  (:require [clojure.test :refer :all]
            [clojure-cards.core :refer :all :as cards]))

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
    (let [cards [[:clubs :king] [:clubs 10] [:hearts :king] [:hearts 4] [:diamonds :ace] [:diamonds 8] [:spades :6]]
          result (cards/find-pair cards)]
      (is (not (nil? result)))
      (is (= 5 (count result)))
      (is (= [:king :king :ace 10 8] (map last result))))))

(deftest is-a-pair
  (testing "Correctly identifies a pair"
    (is (true? (cards/pair? [[:clubs 4] [:clubs 8] [:hearts 4] [:diamonds 10] [:spades :king]]))))
  (testing "Considers three-of-a-kind to also be a pair"
    (is (true? (cards/pair? [[:clubs 4] [:clubs 8] [:hearts 4] [:diamonds 4] [:spades :king]]))))
  (testing "Correctly does not identify a non-pair"
    (is (false? (cards/pair? [[:clubs 4] [:clubs 8] [:hearts 5] [:diamonds 10] [:spades :king]])))))

(deftest is-two-pair?
  (testing "Correctly identifies two pairs"
    (is (true? (cards/two-pair? [[:clubs 9] [:hearts 10] [:diamonds 9] [:diamonds 8] [:spades 10]]))))
  (testing "Correctly does not identify a hand without two pairs"
    (is (false? (cards/two-pair? [[:clubs 9] [:hearts 10] [:diamonds 9] [:diamonds 8] [:spades :king]])))))

(deftest is-three-of-a-kind?
  (testing "Correcting identifies three of a kind"
    (is (true? (cards/three-of-a-kind? [[:clubs 10] [:clubs 9] [:spades 10] [:hearts :ace] [:diamond 10]]))))
  (testing "Correcting does not identify a hand without three of a kind"
    (is (false? (cards/three-of-a-kind? [[:clubs 10] [:clubs 9] [:spades 10] [:hearts :ace] [:diamond :ace]])))))

(deftest find-a-three-of-a-kind
  (testing "Returns the three-of-a-kind hand from the specified cards when one is present"
    (let [cards [[:clubs :king] [:clubs 10] [:hearts :king] [:hearts 4] [:diamonds :king] [:diamonds 8] [:spades :6]]
          result (cards/find-three-of-a-kind cards)]
      (is (not (nil? result)))
      (is (= 5 (count result)))
      (is (= [:king :king :king 10 8] (map last result))))))

(deftest is-four-of-a-kind?
  (testing "Correcting identifies four of a kind"
    (is (true? (cards/four-of-a-kind? [[:clubs 10] [:clubs 9] [:spades 10] [:hearts 10] [:diamond 10]]))))
  (testing "Correcting does not identify a hand without three of a kind"
    (is (false? (cards/four-of-a-kind? [[:clubs 10] [:clubs 9] [:spades 10] [:hearts :ace] [:diamond :ace]])))))

(deftest find-a-four-of-a-kind
  (testing "Returns the correct cards if they contain four of a kind"
    (let [cards [[:clubs 10] [:clubs 9] [:spades 10] [:hearts 10] [:diamonds 10] [:hearts 3] [:diamonds 5]]
          result (cards/find-four-of-a-kind cards)]
      (is (not (nil? result)))
      (is (every? #(= 10 %) (take 4 (map last result)))))))

(deftest is-a-flush
  (testing "Correctly identify a flush"
    (is (true? (cards/flush? [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:clubs 10]]))))
  (testing "Returns false if not a flush"
    (is (false? (cards/flush? [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:hearts 10]])))))

(deftest find-a-flush
  (testing "Extracts the 5 cards making up a flush from a hand containing one"
    (let [result (cards/find-flush [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:clubs 10]])]
      (is (= #{2 4 6 8 10} (set (map last result)))))))

(deftest is-a-straight
  (testing "Correctly identify a straight with a low ace"
    (is (true? (cards/straight? [[:clubs :ace] [:clubs 2] [:hearts 3] [:spades 4] [:clubs 5]]))))
  (testing "Correctly identify a straight with a high ace"
    (is (true? (cards/straight? [[:clubs 10] [:clubs :jack] [:hearts :queen] [:spades :king] [:clubs :ace]]))))
  (testing "Correctly identify a straight with more than 5 cards"
    (is (true? (cards/straight? [[:clubs 2] [:hearts 3] [:spades 4] [:clubs 5] [:clubs 6] [:spades 7]]))))
  (testing "Returns false if no straight is present"
    (is (false? (cards/straight? [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:hearts 10]])))))

(deftest find-a-straight
  (testing "Extracts the 5 cards making up a straight from a hand containing one"
    (let [result (cards/find-straight [[:clubs 2] [:hearts 4] [:clubs 3] [:clubs :ace] [:spades 5]])]
      (is (= #{:ace 2 3 4 5} (set (map last result))))))
  (testing "Returns nil if the cards to not contain a straight"
    (let [result (cards/find-straight [[:clubs 2] [:hearts 4] [:clubs :queen] [:clubs :king] [:spades 3]])]
      (is (nil? result)))))

(deftest is-a-straight-flush
  (testing "Correctly identify a straight flush"
    (is (true? (cards/straight-flush? [[:clubs 5] [:clubs 9] [:clubs 7] [:clubs 6] [:clubs 8]]))))
  (testing "returns false if there is no straight"
    (is (false? (cards/straight-flush? [[:clubs 5] [:clubs 10] [:clubs 7] [:clubs 6] [:clubs 8]]))))
  (testing "returns false if there is no flush"
    (is (false? (cards/straight-flush? [[:clubs 5] [:hearts 9] [:clubs 7] [:clubs 6] [:clubs 8]])))))

(deftest find-a-straight-flush
  (testing "Correctly returns the straight flush from a hand that includes one"
    (let [result (cards/find-straight-flush [[:clubs 7] [:clubs 5] [:hearts 5] [:clubs 3] [:clubs 6] [:clubs 4] [:diamonds 5]])]
      (is (= #{3 4 5 6 7} (set (map last result))))
      (is (= #{:clubs} (set (map first result))))))
  (testing "Returns nil if no straight flush is present"
    (let [result (cards/find-straight-flush [[:spades 7] [:clubs 5] [:hearts 5] [:clubs 3] [:clubs 6] [:clubs 4] [:diamonds 5]])]
      (is (nil? result)))))

(deftest find-a-royal-flush
  (testing "Correctly returns the royal flush from a hand that includes one"
    (let [result (cards/find-royal-flush [[:clubs :ace] [:clubs :queen] [:hearts :queen] [:clubs 10] [:clubs :king] [:clubs :jack] [:diamonds :queen]])]
      (is (= #{10 :jack :queen :king :ace} (set (map last result))))
      (is (= #{:clubs} (set (map first result))))))
  (testing "Returns nil if no royal flush is present"
    (let [result (cards/find-royal-flush [[:clubs 9] [:clubs :queen] [:hearts :queen] [:clubs 10] [:clubs :king] [:clubs :jack] [:diamonds :queen]])]
      (is (nil? result)))))

(deftest find-a-high-card
  (testing "Correctly returns the cards in the right order when only a high card is present"
    (let [cards [[:clubs 4] [:hearts 8] [:spades 10] [:clubs 9] [:diamonds 3] [:diamonds 5] [:hearts :king]]
          result (cards/find-high-card cards)]
      (is (= [:king 10 9 8 5] (map #(last %) result))))))
