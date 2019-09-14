(ns bananas.core-test
  (:require [clojure.test :refer [deftest is]]
            [bananas.core :as sut]
            [clojure.string :as str]))

(defn cata-helper
  [arg]
  (if (number? arg)
    (inc arg)
    (reduce + arg)))

(deftest cata
  []
  (let [f (sut/cata cata-helper)]
    (is (= 9 (f [1 2 3])))
    (is (= 9 (f [[1] [2] [3]])))
    (is (= 9 (f [[1 [2 [3]]]])))
    (is (= 9 (f #{1 2 3})))))

(defn ana-helper
  [n]
  (if (pos? n)
    [(dec n)]
    n))

(deftest ana
  []
  (let [f (sut/ana ana-helper)]
    (is (= [0] (f 1)))
    (is (= [[0]] (f 2)))
    (is (= [[[0]]] (f 3)))
    (is (= [[[[0]]]] (f 4)))))

