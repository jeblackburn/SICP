(ns ch2.huffmanWithMaps
  (:use clojure.contrib.test-is) )

(defstruct leaf :symbols :weight)
(defstruct branch :symbols :weight :left :right)

(defn
  make-leaf [symbol weight]
  (struct leaf (list symbol) weight))

(deftest make-leaf-constructs-a-leaf
  (is (= (struct leaf '(A) 2) (make-leaf 'A 2))))

(defn leaf? [x]
  (and (nil? (:left x)) (nil? (:right x))))

;(println (make-leaf 'A 4))

(defn make-code-tree [left right]
  (struct branch (concat (:symbols left) (:symbols right)) (+ (:weight left) (:weight right)) left right))

(defn choose-branch [bit branch]
  (cond (= bit 0) (:left branch)
        (= bit 1) (:right branch)))

(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
    (if (empty? bits) '()
        (let [next-branch (choose-branch (first bits) current-branch)]
          (if (leaf? next-branch)
              (cons (first (:symbols next-branch))
                    (decode-1 (rest bits) tree))
              (decode-1 (rest bits) next-branch)))))]
  (decode-1 bits tree)))

;(defn decode [bits tree]
;    (if (empty? bits) '()
;        (let [next-branch (choose-branch (first bits) tree)]
;          (if (leaf? next-branch)
;              (cons (first (:symbols next-branch))
;                    (decode (rest bits) tree))
;              (decode (rest bits) next-branch)))))

(defn adjoin-set [x s]
  (println "adjoin-set" x s)
  (cond (empty? s) (list x)
        (< (:weight x) (:weight (first s))) (concat x s)
        :else (concat (first s) (adjoin-set x (rest s)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs) '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair)) ;symbol, frequency
                  (make-leaf-set (rest pairs))))))

(def sample-tree
  (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2)
  (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(println "sample tree: "sample-tree)

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(deftest sample-decodes-to-value
  (is (= (decode sample-message sample-tree) '(A D A B B C A))))


(def left (struct leaf '(A) 2))
(def right (struct leaf '(B) 3))
(def test-tree (struct branch (list 'A 'B) 5 left right))

(deftest leaf?-identifies-a-leaf
  (is (= true (leaf? (make-leaf 'A 4))))
  (is (= false (leaf? (make-code-tree left right)))))

(deftest make-code-tree-tests
  (is (= test-tree (make-code-tree left right))))

; Exercise 2.68
(defn encode-symbol [symbol tree]
  (cond (leaf? tree) '()
        (some #{symbol} (:symbols (:left tree)))
          (cons 0 (encode-symbol symbol (:left tree)))
        (some #{symbol} (:symbols (:right tree)))
          (cons 1 (encode-symbol symbol (:right tree)))
        :else (throw (new java.lang.IllegalArgumentException))))

(deftest encode-symbol-encodes-sample
  (is (= '(0) (encode-symbol 'A sample-tree)))
  (is (= '(1 0) (encode-symbol 'B sample-tree)))
  (is (= '(1 1 0) (encode-symbol 'D sample-tree)))
  (is (= '(1 1 1) (encode-symbol 'C sample-tree))))

(defn encode [symbols tree]
  (if
    (empty? symbols) '()
    (concat (encode-symbol (first symbols) tree) (encode (rest symbols) tree))))

(deftest encode-encodes-the-data
  (is (= '(0) (encode '(A) sample-tree)))
  (is (= '(0 1 1 0 0 1 0 1 0 1 1 1 0) (encode '(A D A B B C A) sample-tree))))

(defn successive-merge [leaf-nodes]
  (let [first-leaf (first leaf-nodes)
        second-leaf (second leaf-nodes)]
          (make-code-tree first-leaf second-leaf)))

(defn generate-huffman-tree [pairs]
  (println "first inside generate-huffman-tree" (first pairs))
  (println "second inside generate-huffman-tree" (second pairs))
  (println "make-leaf-set inside generate-huffman-tree: "(make-leaf-set pairs))
  (successive-merge (make-leaf-set pairs)))

(def sample-tree-two-leaf
  (make-code-tree (make-leaf 'A 4) (make-leaf 'B 2)))

(println "sample tree 2 leaf: " sample-tree-two-leaf)

(def sample-tree-three-leaf
  (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2) (make-leaf 'C 1))))

(println "sample tree 3 leaf" sample-tree-three-leaf)

(deftest generate-huffman-tree-tests
  (is (= sample-tree-two-leaf (generate-huffman-tree '((A 4) (B 2))))))

(run-tests)