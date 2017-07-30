(ns scheduler.counter-test
  (:require [scheduler.component.shaper :refer :all]
            [spyscope.core :refer :all]
            [clojure.test :refer :all]))

(def cnt (->counter 100))

(deftest counter-test 
  (testing "decrease cnt test"
    (is (= (decrease cnt -100)
           (->counter 200)))
    (is (= (decrease cnt -100 150)
           (->counter 150)))
    (is (= (decrease cnt 100 150 50)
           (->counter 50)))
  )
  
  (testing "threshold function test"
    (is (= (threshold cnt 90)
           1))
    (is (= (threshold cnt 110)
           0))
    (is (= (threshold cnt [90 110])
           1))
    (is (= (threshold cnt [80 90 110])
           2))
    (is (= (threshold cnt [80 90 100])
           3))
    (is (= (threshold cnt [80 90 99])
           3))
           ))