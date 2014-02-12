(ns automata.core
  (:use [clojure.pprint]))

(def foo [[:start 1 :free]
          [1 2 \a]
          [2 1 \b]])

(def acceptingness {1 false
                    2 true})

(defn id [rule]
  (nth rule 0))

(defn next-id [rule]
  (nth rule 1))

(defn expected-char [rule]
  (nth rule 2))

(defn accepting [rule]
  (nth rule 3))

(accepting [1 2 \a false])

(defn accepting-by-id [rules id-number]
  (acceptingness id-number))

(defn free-by-id [rules id-number]
  (= :free
     (nth
       (first (filter
                  (fn [rule] (= (first rule) id-number))
                  rules
                  ))
     2)))

(accepting-by-id foo 1)

(defn next-state [rules state input]
  (defn check-rule [rule]
            (and (= (id rule) state)
                 (or
                    (= (expected-char rule) :free)
                    (= (expected-char rule) input)
                  )))
  (nth (first (filter check-rule rules)) 1))

(next-state foo 1 \a)
(next-state foo 2 \b)

foo

(defn run-dfa [rules input & [state last_log]]
  (if (not= (count input) 0)
    (let [goto (next-state rules (or state :start) (first input))
          log (cons [(first input) goto] (or last_log (list [:start state])))
          remaining-input (if (free-by-id rules (or state :start)) input (rest input))]
      (if-not (nil? goto)
          (recur rules remaining-input [goto log])))
    (list (accepting-by-id rules state) (reverse last_log))))

(defn main []
  (pprint (run-dfa foo "ababababababababababababababababababa")))

(first "abab")
(rest "abab")
