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

(defn free [rule]
  (= :free (expected-char rule)))

(defn free-by-id [rules id-number]
  (free (rule-by-id rules id-number)))

(accepting-by-id foo 1)

(defn check-rule [rule state next-char]
  (and (= (id rule) state)
       (or
          (free rule)
          (= (expected-char rule) next-char)
        )))

(defn matched-rules [rules state input]
  (filter #(check-rule % state (first input)) rules))

(defn next-states-and-inputs [rules state input & [last-log]]
  (let [next-states (matched-rules rules state input)]
    (map (fn [nxt]
            (let [log (cons [(first input) (next-id nxt)] (or last-log (list [:start nil])))
                  retval [(next-id nxt) (if (free-by-id rules state) input (rest input)) log]]
              #_(pprint last-log)
              #_(print "\n")
              retval))
          next-states)))

(defn next-states [rules state input]
  (map #(% 0) (next-states-and-inputs rules state input)))

(defn next-inputs [rules state input]
  (map #(% 1) (next-states-and-inputs rules state input)))

; this should be a wrapper around run-nfa
(defn run-dfa [rules input & [state last_log]]
  (if (not= (count input) 0)
    (let [states-and-inputs (next-states-and-inputs rules (or state :start) input)
          state-and-input (first states-and-inputs)
          goto (state-and-input 0)
          log (cons [(first input) goto] (or last_log (list [:start state])))
          remaining-input (state-and-input 1)]
      (if-not (nil? goto)
          (recur rules remaining-input [goto log])))
    (list (accepting-by-id rules state) (reverse last_log))))

(defn repeat-dfa [dfa] ; this actually makes nfas?
  (defn munge-rule [[id next-id expected-char accepting]]
    [id :start :free accepting])
  (let [final-state-rules (map munge-rule (filter accepting dfa))]
    (reduce conj dfa final-state-rules)))

; this will actually run DFAs just fine, but is built to support NFAs.
(defn run-nfa [nfa states-and-inputs]
  (defn reverse-log [[state input log]]
      [state input (reverse log)])
  (let [new-states-and-inputs (mapcat (fn [[& args]] #_(pprint (cons nfa args)) (apply next-states-and-inputs (cons nfa args))) states-and-inputs)
        final-output (map #(% 2) (map reverse-log new-states-and-inputs))]
    #_(pprint new-states-and-inputs)
    (if (some (fn [[state input]]
                (and
                  (not (= state :end)); this is a hack, and should be changed on my next run through this.
                  (accepting-by-id nfa state)
                  (= 0 (count input))))
              new-states-and-inputs)
      (list true final-output)
      (if (some #(not= 0 (count (% 1))) new-states-and-inputs)
        (recur nfa new-states-and-inputs)
        (list false final-output)))))

; this is closer to where we need to end up, but it's not right.
; the problem is that duplicate paths are only really duplicate if
; there's a character consuming rule and a free rule from the same
; source to the same target. counting that is beyond me now. D=
(defn rule-counts [automata]
  (defn id-and-expected-char [rule]
    [(id rule) (expected-char rule)])
  (defn rule-freq []
    (let [ids (distinct (map id automata))
          rules-by-ids (group-by #(identity [(id %) (next-id %)]) automata)
          foo (map #(identity [(key %) (val %)]) rules-by-ids)]
      rules-by-ids))
  (let [ids-and-expected-chars (map id-and-expected-char automata)]
    (vals (rule-freq))))

; need to add a test for the case where there are repeated rules.
(defn is-dfa? [automata]
  (every? #(= % 1) (rule-counts automata)))

(rule-counts (repeat-dfa single-foo))
(is-dfa? (repeat-dfa single-foo))

(defn is-nfa? [automata]
  (some #(not= % 1) (rule-counts automata)))

(defn main []
  ; (pprint foo)
  ; (pprint (run-dfa foo "ababababababababababababababababababa"))
  ; (identity (repeat-dfa single-foo))
  ; (pprint (cons (repeat-dfa single-foo) (list :start "ababababababababababababababababababa")))
  (pprint (run-nfa (repeat-dfa single-foo) [(list :start "ababababababababababababababababababa")]))
  (pprint (is-nfa? single-foo))
  (pprint (is-nfa? (repeat-dfa single-foo)))
  (pprint (is-dfa? single-foo))
  (pprint (is-dfa? (repeat-dfa single-foo)))
  )

(first "abab")
(rest "abab")
