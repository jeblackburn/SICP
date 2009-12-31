(ns Ch1b)

(defn abs [x]
  (if (< x 0) (- x) x)
  )

(defn average [x y]
  (/ (+ x y) 2)
)

(defn square [x] (* x x ))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.000001)
)

(defn improve [guess x]
  (average guess (/ x guess))
)

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defn sqrt [radicand]
  (sqrt-iter 1.0 radicand)
  )

(println (sqrt 16))


