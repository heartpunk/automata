(ns automata.core)

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

(defn run-dfa [rules state input]
  (if (not= (count input) 0)
    (let [goto (next-state rules state (first input))]
      (if-not (nil? goto)
        (recur rules goto (rest input))))
    (accepting-by-id rules state)))

(run-dfa foo 1 "abab")

(first "abab")
(rest "abab")