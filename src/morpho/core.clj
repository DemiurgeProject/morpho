(ns morpho.core)

(require '[clojure.core.match :refer [match]])

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

(defn generate
  ""
  [axiom rule-set]
  )

