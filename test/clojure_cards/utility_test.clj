(ns clojure-cards.utility-test
  (:require [clojure.test :refer :all]
            [clojure-cards.core :refer :all :as cards]))

(deftest compare-ranks
  (testing "three is higher than 2"
    (is (> (rank->integer :3) (rank->integer :2))))
  (testing "jack is higher than 10"
    (is (> (rank->integer :jack) (rank->integer :10))))
  (testing "ace is higher than king"
    (is (> (rank->integer :ace) (rank->integer :king))))
  (testing "ace can be lower than two"
    (is (< (rank->integer false :ace) (rank->integer :2)))))

(deftest group-some-cards-and-sort-by-rank
  (testing "all singles"
    (let [cards [[:clubs :2] [:hearts :king] [:spades :4]]
          expected [[[:hearts :king]] [[:spades :4]] [[:clubs :2]]]]
      (is (= expected (group-and-sort-by-rank cards)))))
  (testing "a pair"
    (let [cards [[:clubs :2] [:hearts :king] [:spades :2] [:spades :4]]
          expected [[[:clubs :2] [:spades :2]] [[:hearts :king]] [[:spades :4]]]]
      (is (= expected (group-and-sort-by-rank cards))))))

(deftest increment-a-card-rank
  (testing "A two becomes a three"
    (is (= [:clubs :3] (cards/inc-rank [:clubs :2]))))
  (testing "A three becomes a four"
    (is (= [:clubs :4] (cards/inc-rank [:clubs :3]))))
  (testing "A four becomes a five"
    (is (= [:clubs :5] (cards/inc-rank [:clubs :4]))))
  (testing "A five becomes a six"
    (is (= [:clubs :6] (cards/inc-rank [:clubs :5]))))
  (testing "A six becomes a seven"
    (is (= [:clubs :7] (cards/inc-rank [:clubs :6]))))
  (testing "A seven becomes a eight"
    (is (= [:clubs :8] (cards/inc-rank [:clubs :7]))))
  (testing "A eight becomes a nine"
    (is (= [:clubs :9] (cards/inc-rank [:clubs :8]))))
  (testing "A nine becomes a 10"
    (is (= [:clubs :10] (cards/inc-rank [:clubs :9]))))
  (testing "A 10 beomes a jack"
    (is (= [:hearts :jack] (cards/inc-rank [:hearts :10]))))
  (testing "A jack beomes a queen"
    (is (= [:hearts :queen] (cards/inc-rank [:hearts :jack]))))
  (testing "A queen beomes a king"
    (is (= [:hearts :king] (cards/inc-rank [:hearts :queen]))))
  (testing "A king beomes an ace"
    (is (= [:hearts :ace] (cards/inc-rank [:hearts :king]))))
  (testing "A ace beomes a 2"
    (is (= [:hearts :2] (cards/inc-rank [:hearts :ace])))))

(deftest find-missing-cards
  (testing "Returns the missing values"
    (let [cards1 [[:clubs :4] [:hearts :king]]
          cards2 [[:hearts :king] [:diamonds :10]]]
      (is (= [[:clubs :4]] (missing cards1 cards2))))))

(deftest sort-some-cards
  (testing "Sort cards of different rank"
    (let [cards [[:clubs :5] [:hearts :ace] [:diamonds :7] [:spades :jack]]
          expected [[:hearts :ace] [:spades :jack] [:diamonds :7] [:clubs :5]]
          result (sort-cards cards)]
      (is (= expected result)))))

(deftest test-sortable
  (testing "Appends at integer after the card"
    (let [cards [[:clubs :2] [:spades :ace]]
          expected [[[:clubs :2] 0] [[:spades :ace] 12]]
          result (sortable cards)]
      (is (= expected result))))
  (testing "Aces low sortable"
    (let [cards [[:clubs :2] [:spades :ace]]
          expected [[[:clubs :2] 0] [[:spades :ace] -1]]
          result (sortable false cards)]
      (is (= expected result)))))

(deftest test-append-sequence-identifier
  (testing "Adds correct values"
    (let [cards [[[:clubs :9] 7] [[:spades :8] 6] [[:hearts :6] 4] [[:clubs :4] 2]]
          expected [[[[:clubs :9] 7] 1] [[[:spades :8] 6] 1] [[[:hearts :6] 4] 2] [[[:clubs :4] 2] 3]]
          result (append-sequence-identifier cards)]
      (is (= (map last expected) (map last result)))))
  (testing "Long sequence"
    (let [cards [[[:clubs :king] 11] [[:spades :queen] 10] [[:hearts :jack] 9] [[:clubs :10] 8] [[:spades :9] 7]]
          expected [[[[:clubs :king] 11] 1] [[[:spades :queen] 10] 1] [[[:hearts :jack] 9] 1] [[[:clubs :10] 8] 1] [[[:spades :9] 7] 1]]
          result (append-sequence-identifier cards)]
      (is (= (map last expected) (map last result))))))

;(deftest get-a-full-sequence (testing "Returns the cards in sequence if they are all in a sequence" (let [cards [[:clubs :5] [:hearts :7] [:spades :3] [:diamonds :6] [:clubs :4]] expected [[:hearts :7] [:diamonds :6] [:clubs :5] [:clubs :4] [:spades :3]]
;          result (get-full-sequence cards)]
;      (is (= expected result))))
;  (testing "Returns nil if all cards are not part of a sequence"
;    (let [cards [[:clubs :ace] [:hearts :7] [:spades :3] [:diamonds :6] [:clubs :4]]
;          result (get-full-sequence cards)]
;      (is (nil? result))))
;  (testing "Returns nil if a duplicate rank is present"
;    (let [cards [[:clubs :5] [:hearts :7] [:spades :5] [:diamonds :6] [:clubs :4]]
;          result (get-full-sequence cards)]
;      (is (nil? result)))))
