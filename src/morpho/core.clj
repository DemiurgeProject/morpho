(ns morpho.core)

(require '[clojure.core.match :refer [match]])
(use 'clojure.walk)

(defn rule
  "Constructs a rule. The first argument is predecessor and the second is successor.
  Predecessor is a vertor with three values: nil or some symbols, a non-terminal symbol, nil or some symbols.
  Guard, probability and priority are optional
  and should be provided as a key-value pairs at the end of the argument list
  as :guard, :prob and :priority correspondingly. The default values are:
  :guard true
  :prob nil
  :priority 0"
  [[pre sym post]
   successor
   & {:keys [guard prob priority] :or {guard true prob nil priority 0}}]
  [priority [pre sym post] guard successor prob])


(defn rule-set
  ""
  [& rules]
  rules)

(defn get-prob [rule] (last rule))

(defn get-priority [rule] (first rule))

(defn set-prob [rule value] (assoc rule 4 value))

(defn group-by-priority
  "Groups a rule-set by the priority of rules"
  [rule-set]
  (group-by get-priority rule-set))

(defn validate-correctness
  "Checks if the sum of probabilities of rules in each priority group is 100%
  or whether nil probs can be set so that the sum is 100%."
  [rule-set]
  (let [groups-by-priority (group-by-priority rule-set)]
    (empty?
      (for [group groups-by-priority
            :let [groups-by-prob-type (group-by #(nil? %)
                                                group)
                  nils (get groups-by-prob-type true)
                  nums (get groups-by-prob-type false)
                  sum-of-probs (apply + (map get-prob nums))]
            :when (or (and (empty? nils)
                           (not= sum-of-probs 100))
                      (and (not (empty? nils))
                           (>= sum-of-probs 100)))]
        :fail))))

(defn set-empty-probs
  "Sets the probabilites of rules with nil values for probability.
  The sum of the probabilities of rules in each priority-group is 100.
  The function returns the rules grouped by priority."
  [rule-set]
  (let [groups-by-priority (group-by-priority rule-set)]
    (for [group (sort-by get-priority (vals groups-by-priority))
          :let [groups-by-prob-type (group-by #(nil? (get-prob %))
                                              group)
                nils (get groups-by-prob-type true)
                nums (get groups-by-prob-type false)
                sum-of-probs (apply + (map get-prob nums))]]
      (if (< sum-of-probs 100)
        (let [prob (/ (- 100 sum-of-probs)
                      (count nils))]
          (vec
            (concat nums
                    (map #(set-prob % prob)
                         nils))))
        group))))

; (defn choose-rule
;   "Takes a seq of rules (for a given priority) and chooses one of the rules."
;   [rule-set]
;   (let [rules-with-shifted-probs (loop [offset 0 traversed [] upcomming rule-set]
;                                    (if (empty? upcomming)
;                                      traversed
;                                      (let [current (first upcomming)
;                                            current-prob (get-prob current)
;                                            new-prob (+ current-prob offset)]
;                                        (recur new-prob (conj traversed (set-prob current new-prob)) (rest upcomming)))))
;         max-prob (apply max (map get-prob rules-with-shifted-probs))
;         r (rand max-prob)]
;     (println rules-with-shifted-probs)
;     (first
;       (sort-by get-prob
;                (filter #(<= (get-prob %) r)
;                        rules-with-shifted-probs)))))

;TODO make choose-rule choose according to the probs
(defn choose-rule
  ""
  [rule-set]
  (rand-nth rule-set))

(defmacro match-with-symbol-on-nth-pos
  ""
  [pos expression]
  )

(defmacro pattern
  ""
  [n predecessor]
  (vec
    (concat (repeat n '_)
            predecessor
            '(& r))))

; (defn pat
;   [n predecessor]
;   (vec))

(defmacro my-match
  ""
  [state predecessor]
  (let [diff (- (count state)
                (count predecessor))]
    (conj (concat
            (apply concat
                   (for [i (range (inc diff))]
                     (list [(list (list 'pattern i predecessor) :seq)] i)))
            '(:else :no-match))
          [state]
          :to-be-changed)))

;TODO find out how to fix the awful mess with my-match needing this to work
(defmacro match-all
  [state predecessor]
  `(eval
     (conj
       (rest (macroexpand-all
               '(my-match ~state ~predecessor)))
       `match)))

(defn generate
  ""
  [axiom rule-set]
  (if (validate-correctness rule-set)
    ()
    (throw (Exception. "The rule-set is not correct: the sum of probabilities is not 100%"))))

