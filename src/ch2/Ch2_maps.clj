(def x {1 2})
;(println (x 1))
;(println (x 3))

(def y {3 4})
(def z {x y})

;(println ((z x) 3))
(defn gcd [a b]
  (if (= b 0)
      a
      (gcd b (rem a b))))

(defstruct ratnum :numerator :denominator)
(defn make-rat [x y]
  (let [g (gcd x y)]
       (struct ratnum (/ x g) (/ y g))))

(defn numer[x]
  (:numerator x)
)

(defn denom[x]
  (:denominator x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println)
  (print (numer x))
  (print "/")
  (print (denom x))
  )

(def one-half (make-rat -1 2))
(print-rat one-half)
(def one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (add-rat one-half one-half))

;(def to-be-squared '(1 2 3 4 5))
;(defn square [& x]
;  (if (nill? x) (* first first))
;  (cons (* (first first))
;        (square (rest x))))

;(map (square to-be-squared))

(defn square [x] (* x x))


(println (filter even? (map square [1 2 3 4 5 6])))