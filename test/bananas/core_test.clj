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

(defn change-helper [x]
  (let [coins [100 50 20 10 5 2 1]]
    (letfn [(lookup [val attr]
              (if (zero? attr)
                (ffirst val)
                (recur (second (first val)) (dec attr))))
            (compress [p]
              (if (seq p)
                (inc (compress (second (first p))))
                0))
            (expand [n]
              (if (<= n 0)
                (vector)
                (vector (expand (dec n)))))
            (separate [pred xs]
              (vector (filter pred xs) (remove pred xs)))]
      (if (= x [])
        1
        (let [given (compress x)
              valid (filter #(<= % given) coins)
              remaining (map #(- given %) valid)
              [zeros to-process] (separate zero? remaining)
              results (apply + (map #(or (lookup x %) 0) to-process))]
          (+ (count zeros) results))))))

(deftest histo
  []
  (let [f (comp (sut/histo change-helper) expand)]
    (is (= 1 (f 0)))
    (is (= 1 (f 1)))
    (is (= 8 (f 19)))
    (is (= 9 (f 20)))
    (is (= 16 (f 21)))))
