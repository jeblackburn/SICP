(ns ch2.sicp_22)
(use 'clojure.contrib.test-is)

;(defn ireverse [x]
;  (let [
;    rev-helper (fn [x r]
;      (if (empty? x)
;        r
;        (recur (rest x) (cons (first x) r))))]
;    (rev-helper x nil)))

(doc list)
(defn ireverse [x]
  (if (empty? x)
    x
    (concat (ireverse (rest x)) (list (first x)))))

(deftest should-reverse-the-list
  (is (= (ireverse(list 1 2 3 4 5)) (list 5 4 3 2 1))))



(def x (cons 1 {2 3}))
(println (first x))
(println (rest x))

(run-tests)