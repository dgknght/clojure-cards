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

(deftest is-four-of-a-kind?
  (testing "Correcting identifies four of a kind"
    (is (true? (cards/four-of-a-kind? [[:clubs 10] [:clubs 9] [:spades 10] [:hearts 10] [:diamond 10]]))))
  (testing "Correcting does not identify a hand without three of a kind"
    (is (false? (cards/four-of-a-kind? [[:clubs 10] [:clubs 9] [:spades 10] [:hearts :ace] [:diamond :ace]])))))

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
