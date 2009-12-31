(ns ch2.section_231
  (:use clojure.contrib.test-is) )

(defn memq [item x]
(cond (empty? x) false
  (= item (first x)) x
  :else (memq item (rest x))))

(deftest memq-returns-the-end-of-a-list
  (is (= (memq 4 [1 2 3 4 5]) [4 5])))

(println (list 'a 'b 'c))
(println (list (list 'george)))
(println (first '((x1 x2) (y1 y2))))
;(pair? (first '(a short list)))
(println (memq 'red '((red shoes) (blue socks))))
(println (memq 'red '(shoes red blue socks)))

(run-tests)