(ns clojure-cards.core
  (:gen-class))

(def suits [:clubs :diamonds :hearts :spades])
(def ranks [:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king])

(defn deck
  "Returns a new unshuffled deck"
  []
  (for [suit suits rank ranks]
    [suit rank]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
