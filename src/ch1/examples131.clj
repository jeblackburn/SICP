(ns ch1.examples131 (:use clojure.contrib.test-is) (:use Ch1b))

(defn cube [x]
;  (println (str "Cubing " x))
  (* x x x))

(defn twice [x] (* 2 x))
;(defn inc [x] (+ 1 x))

(defn sum "Sum the results of function 'term' applied to values between a and b, incremented through function 'next'."[term a next b]
;  (println (str "Summing the results of function " term " for " a " on our way up to " b))
  (if (> a b)
      0
      (+ (term a)
        ; Recursively call sum, passing in the same term function,
        ; invoke next on a, pass in the next function and b.
         (sum term (next a) next b))))

; Exercise 1.30
(defn iter-sum [term a next b]
  (defn iter [a result]
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn iter-sum-cubes [a b]
  (iter-sum cube a inc b))

(deftest iter-sum-equals-recur-sum
  (is (= (sum-cubes 3 9) (iter-sum-cubes 3 9))))

(defn sum-ints [a b]
  (sum identity a inc b))

;(println(sum-ints 1 100))

;intg(f) = [(f*(a + dx/2) +  f*(a + dx + dx/2) + f*(a + 2dx + dx/2)]dx
(defn integral [f a b dx]
;  (defn next-dx [x] (+ x dx))
  (* dx
    (sum f (+ a (/ dx 2)) (fn [x] (+ x dx)) b)))

(println (integral cube 0 1 0.01))
(defn coeff [x n]
  (cond (or (= x 0) (= x n)) 1
     (even? x) 2
     (odd? x) 4))

(defn simpson
  "Calculate the integral of function 'f' between values a and b using Simpson's rule.  'n' must be an even integer."
  [f a b n]
  (defn h [] (/ (- b a) n))
  (defn y [k]
    (* (coeff k n) (f (+ a (* k (h))))))
  (* (/ (h) 3) (sum y 1 inc n)))

;(println (simpson cube 0 1 10.0))
;(println (simpson cube 0 1 1000.0))
;(println (simpson cube 0 1 2000.0))

(defn product [term a next b]
  (if (> a b) 1
     (* (term a) (product term (next a) next b))))

(defn iter-product [term a next b]
  (defn iter [a runningTotal]
;    (println (str "Running total=" runningTotal))
    (if (> a b) runningTotal
       (* (iter (next a) (term a)) runningTotal)))
  (iter a 1))

(deftest should-calculate-product-of-values-through-a-range
  (is (= (* 2 4 6 8 10 12 14 16) (product twice 1 inc 8))))

(deftest should-calculate-iterative-product-of-values-through-a-range
  (is (= (product twice 1 inc 8) (iter-product twice 1 inc 8))))

(defn accumulate [accum-fn term a incrementer b]
  (accum-fn term a incrementer b))

(deftest can-provide-an-accumulation-algorithm-dynamically
  (is (= (product twice 1 inc 8) (accumulate product twice 1 inc 8)))
  (is (= (sum twice 1 inc 10) (accumulate sum twice 1 inc 10))))

(defn fixed-point [f first-guess]
  (let [tolerance 0.00001]
  (defn close-enough? [v1 v2]
    (println (str "Comparing " v1 " and " v2 " against tolerance " tolerance))
    (< (abs (- v1 v2)) tolerance))
  (defn try [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess)))

(defn sqrt2 [x]
  (fixed-point (fn [y] (println (str "Averaging " y " and " (/ x y))) (average y (/ x y)))
               1.0))

(deftest should-find-the-sqrt-of-9
  (is (= (sqrt2 9) 3)))
;(deftest should-find-the-fixed-point-for-cos-1
;  (is (= (fixed-point cos 1.0) .7390822985224023)))

(run-tests)