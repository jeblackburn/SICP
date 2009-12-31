(ns ch2.exercise_29
  (:use clojure.contrib.test-is) )

(defn make-mobile [left right]
  [left right])

(defn left [mobile]
  (first mobile))

(defn right [mobile]
  (fnext mobile))

(deftest should-construct-a-mobile
  (is (= 10 (left (make-mobile 10 5))))
  (is (= 5 (right (make-mobile 10 5)))))

(run-tests)