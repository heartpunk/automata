(ns automata.core
  (:use [clojure.pprint]))

(def foo [[1 2 \a]
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

(accepting-by-id foo 1)

(defn next-state [rules state input]
  (defn check-rule [rule]
            (and (= (id rule) state)
                 (= (expected-char rule) input)))
  (nth (first (filter check-rule rules)) 1))

(next-state foo 1 \a)
(next-state foo 2 \b)

foo

(defn run-dfa [rules state input & last_log]
  (if (not= (count input) 0)
    (let [goto (next-state rules state (first input))
          log (cons [(first input) goto] (or last_log (list [:start state])))]
      (if-not (nil? goto)
        (recur rules goto (rest input) log)))
    (list (accepting-by-id rules state) (reverse last_log))))

(defn main []
  (pprint (run-dfa foo 1 "ababababababababababababababababababa")))

(first "abab")
(rest "abab")
