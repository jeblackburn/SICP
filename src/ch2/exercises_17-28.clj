(ns ch2.exercises_17-28  (:use clojure.contrib.test-is) )

; 2-17
(defn last-pair [x]
  (if (empty? (rest x))
    x
    (last-pair (rest x))))

(deftest last-pair-returns-last-element-of-a-list
  (is (= (list 5) (last-pair (list 1 2 3 4 5)))))

; 2-18
(defn my-reverse [array]
  (defn my-reverse-iter [input output]
  (if (first input)
    (my-reverse-iter (rest input) (cons (first input) output))
    output))
  (my-reverse-iter array []))

(deftest my-reverse-reverses-a-list
  (is (= (list 5 4 3 2 1) (my-reverse (list 1 2 3 4 5)))))

(defn reverse-multi-impl [array]
  (loop [input array output []]
  (if (first input)
    (recur (rest input) (cons (first input) output))
    output)))

(defmulti reverse-multi class)

(defmethod reverse-multi String [s]
  (apply str (reverse-multi-impl s)))

(defmethod reverse-multi :default [array]
  (reverse-multi-impl array))

(deftest reverse-multi-reverses-a-string
  (is (= "asdfg" (apply str (reverse-multi "gfdsa")))))

(deftest reverse-multi-reverses-a-list
  (is (= (list 1 2 3 4 5) (reverse-multi (list 5 4 3 2 1)))))

; 2-19
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn no-more? [coin-values]
  (empty? coin-values))

(defn except-first-denomination [coin-values]
  (rest coin-values))

(defn first-denomination [coin-values]
  (first coin-values))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))))

; Define the procedures first-denomination, except-first-denomination, and no-more? in terms of primitive operations
; on list structures. Does the order of the list coin-values affect the answer produced by cc? Why or why not?

(deftest cc-should-calculate-the-ways-to-change-a-buck
  (is (= 292 (cc 100 us-coins))))

(deftest cc-doesnt-care-what-order-the-list-is-in
  (is (= 292 (cc 100 (list 1 5 10 25 50)))))

(defn safe-mod [x y]
  (if (nil? x) 0
    (mod x y)))

; 2-20
(defn add-matches [modulo candidates solution]
  (println candidates solution)
  (if (empty?  candidates)
    (do (println "candidates empty")
      (list solution)))
  (add-matches modulo (rest candidates)
    (if (= modulo (mod (first candidates) 2))
      (conj solution (first candidates))
      solution)))

(defn same-parity [& x]
  (add-matches (mod (first x) 2) (rest x) []))

;(deftest same-parity-works-for-odd-numbers
;  (is (= (list 1 3 5 7) (same-parity 1 2 3 4 5 6 7))))

;(deftest same-parity-works-for-even-numbers
;  (is (= (list 2 4 6) (same-parity 2 3 4 5 6 7))))

; 2-21
(defn square [x] (* x x))
(defn square-list [items]
  (if (empty? items)
      nil
      (cons (square (first items)) (square-list (rest items)))))

(defn square-list-map [items]
  (map square items))

(deftest square-list-works
  (is (= (list 1 4 9 16) (square-list (list 1 2 3 4)))))

(deftest square-list-map-works
  (is (= (list 1 4 9 16) (square-list-map (list 1 2 3 4)))))

; 2-22  --  no-op

; 2-23
(defn my-for-each [func items]
  (if (empty? items)
    nil
    (do
      (func (first items))
      (my-for-each func (rest items)))))
; can use recur

;(my-for-each println [1 2 3 4 5])

; 2-24  --  No-op

; 2-25
(deftest find-the-seven-1
  (is (= 7 (fnext (fnext (next (list 1 3 (list 5 7) 9)))))))

(deftest find-the-seven-2
  (is (= 7 (ffirst (list (list 7))))))

(deftest find-the-seven-3
  (is (= 7 (fnext (fnext (fnext (fnext (fnext (fnext (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))

; 2-26
; Go thru these, interesting distinctions
; Vectors are much better for this kind of append operation
;(def x (list 1 2 3))
;(def y (list 4 5 6))
;(println "concat" (concat x y)) ; (1 2 3 4 5 6 )
;(println "conj" (conj x y))     ; ((4 5 6) 1 2 3)
;(println "cons" (cons x y))     ; ((1 2 3) 4 5 6)
;(println "list" (list x y))     ; ((1 2 3) (4 5 6))
;
;(def x2 [1 2 3])
;(def y2 [4 5 6])
;(println "concat vec" (concat x2 y2)) ; (1 2 3 4 5 6 )
;(println "conj vec" (conj x2 y2))     ; [1 2 3 [4 5 6]]
;(println "cons vec" (cons x2 y2))     ; [[1 2 3] 4 5 6]
;(println "list vec" (list x2 y2))     ; ([1 2 3] [4 5 6])

; 2-27
(def x (list (list 1 2) (list 3 4)))
(def x2 (list 0 (list 1 2) (list 3 4) 5))

(defn my-reverse [array]
  (loop [input array output []]
    (if (empty? input)
     output
     (recur (rest input) (cons (first input) output)))))

(deftest my-reverse-works
  (is (= (list (list 3 4) (list 1 2)) (my-reverse x))))

(defn deep-reverse [biglist]
  (loop [mylist biglist output []]
    (if (empty? mylist)
      output
      (recur (next mylist) (cons (my-reverse (first mylist)) output)))))

(defn deep-reverse-mixed [biglist]
  (loop [mylist biglist output []]
    (if (empty? mylist)
      output
      (recur
        (next mylist)
        (cons
        (if (seq? (first mylist))
          (my-reverse (first mylist))
          (first mylist)) output)))))

(deftest deep-reverse-works
  (is (= (list (list 4 3) (list 2 1)) (deep-reverse x))))

(deftest deep-reverse-works-with-mixed-elements
  (is (= (list 5 (list 4 3) (list 2 1) 0) (deep-reverse-mixed x2))))

; 2-28
(def x (list (list 1 2) (list 3 4)))

(defn fringe [x]
  (loop [thelist x output []]
    (if (seq? thelist)
      (concat output (fringe (first thelist)))
      (if (empty? thelist)
        output
        (recur (rest thelist) output)))))

(deftest fringe-lists-leaf-nodes-in-order
  (is (= (list 1 2 3 4) (fringe x))))

(run-tests)