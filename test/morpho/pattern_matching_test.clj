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
           (normalize-pattern {:align :ver :elems [:a :b :c]}))))
  (testing "2 by 2 pattern - case with columns in row"
    (is (= [[:a :b]
            [:c :d]]
           (normalize-pattern {:align :hor :elems [{:align :ver :elems [:a :c]}
                                                   {:align :ver :elems [:b :d]}]}))))
  (testing "2 by 2 pattern - case with rows in column"
    (is (= [[:a :b]
            [:c :d]]
           (normalize-pattern {:align :ver :elems [{:align :hor :elems [:a :b]}
                                                   {:align :hor :elems [:c :d]}]})))))
