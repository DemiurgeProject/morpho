(ns morpho.pattern-matching)
(require '[clojure.core.match :refer [match]])
(require '[defun :refer [defun]])

; Defines 2D pattern matching of irregular rectangular grids.

; Here's how patterns are being constructed:
;   A pattern is a hash of the type:
;     {:align :hor/:ver :elems [subpattern1 subpattern2 ... subpatternN]}
;
;   :align specifies whether the elements are aligned horizontally or vertically
;   :elems are the cells of the pattern
;
;   Examples:
;                  ___
;                 | a |
;                  ---
;     the pattern | b |  will be encoded as {:align :ver :elems [:a :b :c]}
;                  ---
;                 | c |
;                  ---
;
;                  ___________
;                 |   | b |   |
;     the pattern | a |---| c | will be encoded as:
;                 |   | d |   |
;                  -----------     {:align :hor
;                                   :elems [:a
;                                           {:align :ver :elems [:b :d]}
;                                           :c]}

; TODO: delete this
; (defn indexes-of
;   "gets indexes of all occurances of e in coll"
;   [e coll]
;   (keep-indexed #(if (= e %2) %1) coll))

; TODO: delete this
; (defn find-index
;   "gets the index of the first occurance of e in coll"
;   [e coll]
;   (first (indexes-of e coll)))

; TODO: delete this
; (defn first-by
;   "gets the first member of coll that matches predicate"
;   [predicate coll]
;   (first (filter predicate coll)))

; TODO: delete this
; (defn find-index-by
;   "returns the index of the first member of coll that matches the predicate"
;   [predicate coll]
;   (find-index (first-by predicate coll) coll))

; TODO: delete this
; (defn filter-nil-paths
;   "removes all paths that lead to nil values"
;   [index]
;   (remove #(nil? (last %)) index))

; TODO: delete this
; (defn by-rows?
;   "true if the split is by rows"
;   [pattern]
;   (every? coll? pattern))

; TODO: delete this
; (defn build-pattern-index
;   "builds an index structure that is a collection of pairs (<path to a value> <value>)
;   Note: the first pair of the path is meant to be the relative path to the root cell and
;   the next are the path within that (shifted) cell"
;   ([pattern]
;    (build-pattern-index pattern []))
;   ([pattern path]
;    (if (non-coll? pattern)
;      (vector path pattern)
;      (let [by-rows (by-rows? pattern)]
;        (mapcat #(build-pattern-index %1 (conj path (if by-rows
;                                                      [0 %2]
;                                                      [%2 0])))
;                pattern
;                (range (count pattern)))))))

; TODO: delete this
; (defn find-root
;   "finds the root cell: the cell that is minimally nested"
;   [index]
;   (apply (partial min-key
;                   #(count (first %)))
;          index))

; TODO: delete this
; (defn normalize-root
;   "puts the root cell in the first place of the index structure"
;   [index]
;   (let [root (find-root index)]
;     (conj (remove #(= root %) index)
;           root)))

; TODO: delete this
; (defn recalculate-indices
;   "recalculates the relative paths (the first coordinate) so that the root cell is at [0 0]"
;   [index]
;   (let [root (find-root index)
;         root-index (ffirst root)]
;     (map #(assoc-in (vec %)
;                     [0 0]
;                     (vec
;                       (map - (ffirst %) root-index)))
;          index)))

; TODO: delete this
; (defn create-index
;   "Used to create the end pattern index structure. It takes the pattern as an argument. Example pattern is:
;    _____________
;   | x |     | z |
;   |---|  a  |---|
;   | y |     |   |
;    ‾‾‾‾‾‾‾‾‾‾‾‾‾
;   will be written as [[[:x] [:y]] :a [[:z] [nil]]].
;   (vector of :keywords or \"strings\" will be a line with those things as columns, but if they are in brackets, they will be rows)
;   we interpted nil as *empty space*
;   The result of the function for this input is:
;   ([[[0 0]] :a] [[[-1 0] [0 0] [0 0]] :x] [[[-1 0] [0 1] [0 0]] :y] [[[1 0] [0 0] [0 0]] :z])
;   On first position is the root. The other pairs are of the type [[<position relative to the root cell> <list of inner positions>] <value>]"
;   [pattern]
;   (->> (build-pattern-index pattern)
;        (partition 2)
;        (filter-nil-paths)
;        (recalculate-indices)
;        (normalize-root)))

; (defun match-block
;   ""
;   ([_ nil] true)
;   ([block pattern :guard #(= %1 %2)])
;   ([block pattern :guard (and (coll? %1)
;                               (coll? %2))
;                               (= (count %1)
;                                  (count %2))]
;     (not-any? false?
;               (map match-block pattern block)))


; TODO: delete this
; (defn match-block
;   "Tests whether a given block of the state is matching against the pattern.
;   It will match if they are the same (with the additional rule that
;   anything will match against a nil element of the pattern)."
;   [block pattern]
;   (if (and (coll? pattern)
;            (coll? block)
;            (= (count pattern)
;               (count block)))
;     (not-any? false?
;               (map match-block pattern block))
;     (or (= pattern nil)
;         (= pattern block))))

; TODO: delete this
; (defn splits
;   ""
;   [pattern]
;   (if (coll? pattern)
;     (count pattern)
;     1))

; TODO: delete this
; (defn find-index-of-max
;   ""
;   [coll]
;   (apply (partial max-key coll)
;          (range (count coll))))

(defn non-coll? [x]
  (not (coll? x)))


(defun height
  "Returns the number elements if :align :ver or 1 otherwise.
  If the pattern is just a single symbol it will return 1 as well"
  ([(pattern :guard non-coll?)] 1)
  ([{:align :hor}] 1)
  ([{:align :ver :elems coll}] (count coll)))

(defun width
  "Returns the number elements if :align :hor or 1 otherwise
  If the pattern is just a single symbol it will return 1 as well"
  ([(pattern :guard non-coll?)] 1)
  ([{:align :ver}] 1)
  ([{:align :hor :elems coll}] (count coll)))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm2
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcm [& xs] (reduce lcm2 xs))

(defun target-size
  "Returns the size of the smallest cell.
  The format is [x y] which means that it's width is 1/x times the total width
  and it's height is 1/y times the total height"
  ([pattern]
   (target-size pattern 1 1))
  ([(pattern :guard non-coll?) x y] {:x x :y y})
  ([pattern x y]
     (let [size-pairs (map #(target-size % x y)
                           (pattern :elems))
           xs (map #(% :x) size-pairs)
           ys (map #(% :y) size-pairs)]
       {:x (* (width pattern) (apply lcm xs))
        :y (* (height  pattern) (apply lcm ys))})))

(defn n-times
  ""
  [pattern n]
  (if (= n 1)
    pattern
    (vec
      (take n
            (repeat pattern)))))

(defn make-column
  ""
  [pattern n]
  (if (= n 1)
    [pattern]
    (->> (repeat
           (if (coll? pattern)
             pattern
             [pattern]))
         (take n)
         (vec))))

(defn make-row
  ""
  [pattern n]
    (->> (repeat pattern)
         (take n)
         (vec)))

(defn force-concat
  [& xs]
  (mapcat #(if (coll? %) % [%]) xs))

(defn merge-grids-horizontally
  ""
  [grids]
  (vec
    (apply (partial map (comp vec force-concat))
           grids)))

(defn merge-grids-vertically
  ""
  [grids]
  (vec
    (apply concat grids)))

(defun ^:dynamic normalize-pattern
  ""
  ([pattern :guard non-coll?] pattern)
  ([pattern]
   (normalize-pattern pattern
                      (width pattern)
                      (height pattern)
                      (target-size pattern)))
  ([pattern x y target]
   (let [x-multiple (/ (target :x) x)
         y-multiple (/ (target :y) y)]
     (if (non-coll? pattern)
       (-> (make-row pattern x-multiple)
           (make-column y-multiple))
       (let [inners (map #(normalize-pattern %
                                             (* x (width %))
                                             (* y (height %))
                                             target)
                         (pattern :elems))]
         (case (pattern :align)
               :hor (merge-grids-horizontally inners)
               :ver (merge-grids-vertically inners)))))))
