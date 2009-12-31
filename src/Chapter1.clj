(use 'clojure.contrib.test-is)

(defn sum-of-squares [x y]
  (+ (* x x ) (* y y)))

(defn find-larger [x y]
  (if (> x y) x y))

(defn find-largest[x y z]
  (find-larger (find-larger x y) (find-larger y z))
  )

(defn sum-largest-squares [x y z]
  (sum-of-squares (find-larger x y) (find-larger y z)))

(deftest sum-of-squares-tests
  (is (= 25 (sum-of-squares 3 4)))
  (is (= 25 (sum-of-squares -3 -4))))

(deftest find-larger-tests
  (is (= 5 (find-larger 4 5)))
  (is (= 5 (find-larger 5 4)))
  (is (= 5 (find-larger 5 5)))
  )

(deftest sum-largest-squares-tests
  (is (= (+ 25 16) (sum-largest-squares 3 4 5)))
  (is (= (+ 25 16) (sum-largest-squares 5 4 3)))
  (is (= (+ 36 25) (sum-largest-squares 4 5 6)))
  (is (= (+ 25 25) (sum-largest-squares 4 5 5)))
  )

(defn add2[x](+ x 2))
(deftest add2-test
  (is (= 7 (add2 5))))

(run-tests)