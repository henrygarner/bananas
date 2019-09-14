(ns bananas.core-test
  (:require [clojure.test :refer [deftest is]]
            [bananas.core :as sut]))

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
