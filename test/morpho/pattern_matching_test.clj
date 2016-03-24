(ns morpho.pattern-matching-test
  (:require [clojure.test :refer :all]
            [morpho.pattern-matching :refer :all]))

(deftest normalize-pattern-test
  (testing "pattern is just a single row"
    (is (= [[:a :b :c]]
           (normalize-pattern {:align :hor :elems [:a :b :c]}))))
  (testing "pattern is just a single column"
    (is (= [[:a]
            [:b]
            [:c]]
           (normalize-pattern {:align :ver :elems [:a :b :c]})))))
