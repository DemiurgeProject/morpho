(ns morpho.pattern-matching)

; Defines 2D pattern matching of irregular rectangular grids.

; Here's how patterns are being constructed:
;   If all elements of a vector are vectors themselves, then they form a column.
;     Example:
;                  ___
;                 | a |
;                  ---
;     the pattern | b |  will be encoded as [[:a] [:b] [:c]]
;                  ---
;                 | c |
;                  ---
;
;   Otherwise, they form a row.
;     Example:
;                  ___________
;                 |   | b |   |
;     the pattern | a |---| c | will be encoded as [:a [[:b] [:d]] :c]
;                 |   | d |   |
;                  -----------

(defn indexes-of
  "gets indexes of all occurances of e in coll"
  [e coll]
  (keep-indexed #(if (= e %2) %1) coll))

(defn find-index
  "gets the index of the first occurance of e in coll"
  [e coll]
  (first (indexes-of e coll)))

(defn first-by
  "gets the first member of coll that matches predicate"
  [predicate coll]
  (first (filter predicate coll)))

(defn find-index-by
  "returns the index of the first member of coll that matches the predicate"
  [predicate coll]
  (find-index (first-by predicate coll) coll))

(defn non-coll? [x]
  (not (coll? x)))

(defn filter-nil-paths
  "removes all paths that lead to nil values"
  [index]
  (remove #(nil? (last %)) index))

(defn build-pattern-index
  "builds an index structure that is a collection of pairs (<path to a value> <value>)
  Note: the first pair of the path is meant to be the relative path to the root cell and
  the next are the path within that (shifted) cell"
  ([pattern]
   (build-pattern-index pattern []))
  ([pattern path]
   (if (non-coll? pattern)
     (vector path pattern)
     (let [by-rows (every? coll? pattern)]
       (mapcat #(build-pattern-index %1 (conj path (if by-rows
                                                     [0 %2]
                                                     [%2 0])))
               pattern
               (range (count pattern)))))))

(defn find-root
  "finds the root cell: the cell that is minimally nested"
  [index]
  (apply (partial min-key
                  #(count (first %)))
         index))

(defn normalize-root
  "puts the root cell in the first place of the index structure"
  [index]
  (let [root (find-root index)]
    (conj (remove #(= root %) index)
          root)))

(defn recalculate-indices
  "recalculates the relative paths (the first coordinate) so that the root cell is at [0 0]"
  [index]
  (let [root (find-root index)
        root-index (ffirst root)]
    (map #(assoc-in (vec %)
                    [0 0]
                    (vec
                      (map - (ffirst %) root-index)))
         index)))

(defn create-index
  "Used to create the end pattern index structure. It takes the pattern as an argument. Example pattern is:
   _____________
  | x |     | z |
  |---|  a  |---|
  | y |     |   |
   ‾‾‾‾‾‾‾‾‾‾‾‾‾
  will be written as [[[:x] [:y]] :a [[:z] [nil]]].
  (vector of :keywords or \"strings\" will be a line with those things as columns, but if they are in brackets, they will be rows)
  we interpted nil as *empty space*
  The result of the function for this input is:
  ([[[0 0]] :a] [[[-1 0] [0 0] [0 0]] :x] [[[-1 0] [0 1] [0 0]] :y] [[[1 0] [0 0] [0 0]] :z])
  On first position is the root. The other pairs are of the type [[<position relative to the root cell> <list of inner positions>] <value>]"
  [pattern]
  (->> (build-pattern-index pattern)
       (partition 2)
       (filter-nil-paths)
       (recalculate-indices)
       (normalize-root)))


(defn height
  "returns the number of top-level rows in the pattern"
  [pattern]
  (if (= (count pattern)
         (count (filter vector? pattern))
         (count pattern)
         1)))

(defn width
  "returns the number of top-level columns in the pattern"
  [pattern]
  (->> (filter vector? pattern)
       (map count)
       (max)))

(defn match-block
  "Tests whether a given block of the state is matching against the pattern.
  It will match if they are the same (with the additional rule that
  anything will match against a nil element of the pattern)."
  [block pattern]
  (if (and (coll? pattern)
           (coll? block)
           (= (count pattern)
              (count block)))
    (not-any? false?
              (map match-block pattern block))
    (or (= pattern nil)
        (= pattern block))))

(defn match
  ""
  [state pattern]
  (let [height (height pattern)
        width (width pattern)]))
