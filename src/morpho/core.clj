(ns morpho.core)

; (require '[clojure.core.match :refer [match]])
; (use 'clojure.walk)

;TODO: guards don't work, I think - check this

(defn rule
  "Constructs a rule. The first argument is predecessor and the second is successor.
  Predecessor is a vector with symbols (at least one non-terminal)
  Guard, probability and priority are optional
  and should be provided as a key-value pairs at the end of the argument list
  as :guard, :prob and :priority correspondingly. The default values are:
  :guard true
  :prob nil
  :priority 0"
  ; [[pre sym post]
  [predecessor
   successor
   & {:keys [guard prob priority] :or {guard true prob nil priority 0}}]
  [priority predecessor guard successor prob])


(defn rule-set
  ""
  [& rules]
  rules)

(defn get-prob [rule] (last rule))

(defn get-priority [rule] (first rule))

(defn set-prob [rule value] (assoc rule 4 value))

(defn get-predecessor [rule] (nth rule 1))

(defn get-successor [rule] (nth rule 3))

(defn group-by-priority
  "Groups a rule-set by the priority of rules"
  [rule-set]
  (group-by get-priority rule-set))

(defn group-by-predecossor
  "Groups a rule-set by the predecessor of rules"
  [rule-set]
  (group-by get-predecessor rule-set))

(defn validate-correctness
  "Checks if the sum of probabilities of rules in each priority group is 100%
  or whether nil probs can be set so that the sum is 100%."
  [rule-set]
  (let [groups-by-priority (group-by-priority rule-set)]
    (empty?
      (for [[_ group] groups-by-priority
            :let [groups-by-prob-type (group-by #(nil? (get-prob %))
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
  The sum of the probabilities of rules with same predecessor in each priority-group is 100.
  The function returns the rules grouped by priority."
  [rule-set]
  (apply merge-with concat
         (let [groups-by-priority (group-by-priority rule-set)]
           (for [priority-group (vals groups-by-priority) ; TODO check if this sorting really works
                 group (vals (group-by-predecossor priority-group))
                 :let [groups-by-prob-type (group-by #(nil? (get-prob %))
                                                     group)
                       nils (get groups-by-prob-type true)
                       nums (get groups-by-prob-type false)
                       sum-of-probs (apply + (map get-prob nums))]]
             (if (< sum-of-probs 100)
               (let [prob (/ (- 100 sum-of-probs)
                             (count nils))
                     value (vec
                             (concat nums
                                     (map #(set-prob % prob)
                                          nils)))]
                 (hash-map (get-priority (first value))
                           value))
               group)))))

(defn in-range?
  "checks if number is in [start; end)"
  [number start end]
  (and (<= start number)
       (< number end)))

(defn choose-rule
  "chooses a rule from the rules in a priority group based on their probabilities"
  [rule-set]
  (let [dice (rand 100)]
    (loop [offset 0
           [current-rule & rules] rule-set]
      (let [current-prob (get-prob current-rule)
            end (+ current-prob offset)]
      (if (in-range? dice offset end)
        current-rule
        (recur end rules))))))

(defn match
  ""
  [state pattern]
  (let [pattern-len (count pattern)
        state-len (count state)
        diff (- state-len pattern-len)]
    (if-not (neg? diff)
      (first
        (for [i (range (inc diff))
              :when (= pattern
                       (take pattern-len
                             (drop i state)))]
          i)))))

(defn new-state
  ""
  [state rule]
  (let [match-pos (match state
                         (get-predecessor rule))
        pre (take match-pos state)
        post (drop (+ match-pos
                      (count (get-predecessor rule)))
                   state)]
    (concat pre
            (get-successor rule)
            post)))

(defn matches?
  ""
  [state rule]
  (not
    (nil?
      (match state
             (get-predecessor rule)))))

(defn generate
  ""
  ([axiom rule-set]
  (if (validate-correctness rule-set)
    (let [max-priority (apply max
                              (map get-priority
                                   rule-set))
          rules (set-empty-probs rule-set)]
      (generate axiom rules max-priority))
    (throw (Exception. "The rule-set is not correct: the sum of probabilities is not 100%"))))
  ([axiom rules max-priority]
   (loop [state axiom priority max-priority]
     (if (empty? (filter string? state))
       state
       (let [matches-state? (partial matches? state)
             rule (choose-rule
                    (filter matches-state?
                            (get rules priority)))]
         (if (not (nil? rule))
           (recur (new-state state rule) max-priority)
           (if (= priority 0)
             (list :no-rule-matches-error state)
             (recur state (dec priority)))))))))

