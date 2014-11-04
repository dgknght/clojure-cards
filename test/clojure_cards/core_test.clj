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

(deftest is-a-flush
  (testing "Correctly identify a flush"
    (is (true? (cards/flush? [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:clubs 10]]))))
  (testing "Returns false if not a flush"
    (is (nil? (cards/flush? [[:clubs 2] [:clubs 4] [:clubs 6] [:clubs 8] [:hearts 10]])))))
