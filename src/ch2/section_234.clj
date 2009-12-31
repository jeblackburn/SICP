(ns ch2.section_234
  (:use clojure.contrib.test-is) )

;(defn equals-three? [x] (= 3 x))
;(println (some #{3} '(1 2 3 4 5)))

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn leaf? [object]
  (= (first object) 'leaf))

(defn symbol-leaf [x] (second x))

(defn weight-leaf [x] (nth x 2))

(deftest leaf-values-are-correct
  (is (= 2 (weight-leaf (make-leaf 1 2))))
  (is (= 1 (symbol-leaf (make-leaf 1 2))))
  (is (= true (leaf? (make-leaf 1 2)))))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn symbols [node]
;  (println "symbols" node)
  (if (leaf? node)
    (list (symbol-leaf node))
    (nth node 2)))

(defn make-code-tree [left right]
  (list left
    right
    (concat (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))

(def left-leaf (make-leaf 2 1))
(def right-leaf (make-leaf 5 3))

(deftest make-code-tree-creates-a-tree
  (is (= (make-code-tree left-leaf right-leaf)
         (list left-leaf right-leaf '(2 5) 4))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)))

(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
    (if (empty? bits) '()
        (let [next-branch (choose-branch (first bits) current-branch)]
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (rest bits) tree))
              (decode-1 (rest bits) next-branch)))))]
  (decode-1 bits tree)))

(defn adjoin-set [x s]
  (cond (empty? s) (list x)
        (< (weight x) (weight (first s))) (cons x s)
        :else (cons (first s) (adjoin-set x (rest s)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs) '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair)) ;symbol, frequency
                  (make-leaf-set (rest pairs))))))

; Exercise 2.67
(def sample-tree
  (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2)
  (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(println "sample tree: "sample-tree)

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(deftest sample-decodes-to-value
  (is (= (decode sample-message sample-tree) '(A D A B B C A))))

; Exercise 2.68 
(defn encode-symbol [symbol tree]
  (cond (leaf? tree) '()
        (some #{symbol} (symbols (left-branch tree)))
          (cons 0 (encode-symbol symbol (left-branch tree)))
        (some #{symbol} (symbols (right-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree)))
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

;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(def sample-tree-two-leaf
  (make-code-tree (make-leaf 'A 4) (make-leaf 'B 2)))

(println "sample tree 2 leaf: " sample-tree-two-leaf)

(def sample-tree-three-leaf
  (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2) (make-leaf 'C 1))))

(println "sample tree 3 leaf" sample-tree-three-leaf)

(deftest generate-huffman-tree-tests
  (is (= sample-tree-two-leaf (generate-huffman-tree '((A 4) (B 2))))))

(run-tests)
