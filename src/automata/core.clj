(ns automata.core
  (:use [clojure.pprint]))

(def foo [[:start 1 :free false]
          [1 2 \a false]
          [2 1 \b true]])

(def single-foo
         [[:start 1 :free false]
          [1 2 \a false]
          [2 :end \b true]])

(defn id [rule] (rule 0))

(defn next-id [rule] (rule 1))

(defn expected-char [rule] (rule 2))

(defn accepting [rule] (rule 3))

(defn start [rule] (= (id rule) :start))

(defn rule-by-id [rules id-number]
  (first (filter (fn [rule] (= (first rule) id-number))
                  rules)))

(defn accepting-by-id [rules id-number]
  (accepting (rule-by-id rules id-number)))

(defn free-by-id [rules id-number]
  (= :free
     (nth (rule-by-id rules id-number) 2)))

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

(defn repeat-dfa [dfa]
  (defn munge-rule [[id next-id expected-char accepting]]
    [id :start :free accepting])
  (let [final-state-rules (map munge-rule (filter accepting dfa))]
    (reduce conj dfa final-state-rules)))

(defn main []
  (pprint foo)
  (pprint (run-dfa foo "ababababababababababababababababababa"))
  (pprint (repeat-dfa single-foo))
  (pprint (run-dfa (repeat-dfa single-foo) "ababababababababababababababababababa"))
  )

(first "abab")
(rest "abab")
