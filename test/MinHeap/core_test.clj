(ns min-heap.core-test
  (:require [clojure.test :refer :all]
            [min-heap.zip :refer :all]
            [clojure.zip :as zip]))

(deftest tree-node-test
  (testing "Initial tree"
  (is (= min-heap) nil)))

(deftest min-heap-insert-test
  (testing "min heap insertion"
    (are [min-heap n answer] (= answer (min-heap-insert min-heap n))
         nil 5 [5 nil nil]
         nil nil nil
         nil "nil" ["nil" nil nil]
         nil "" ["" nil nil]
         [72 [81 nil nil] nil] -5 [-5 [81 nil nil] [72 nil nil]]
         [-1 [81 nil nil] nil] 0 [-1 [81 nil nil] [0 nil nil]]
         [25 [50 nil nil] nil] 25 [25 [50 nil nil] [25 nil nil]]
         ["aing" ["going" ["having" nil nil] nil] ["from" ["hello" nil nil] nil]] "bye" ["aing" ["bye" ["having" nil nil] ["going" nil nil]] ["from" ["hello" nil nil] nil]]
         [45 [65 [72 [81 nil nil] nil] [82 [95 nil nil] nil]] [87 [90 [99 nil nil] nil] [96 nil nil]]] 50 [45 [50 [65 [81 nil nil] [72 nil nil]] [82 [95 nil nil] nil]] [87 [90 [99 nil nil] nil] [96 nil nil]]]
         )))

(deftest zipper->left-child-test
  (testing "Get left child node"
    (are [zipper answer] (= answer (zip/node (zipper->left-child zipper)))
         (zip/vector-zip [5 nil nil]) nil
         (zip/vector-zip [5 10 nil]) 10
         (zip/vector-zip ["this" ["is" "big"] "match"]) ["is" "big"])))

(deftest zipper->right-child-test
  (testing "Get right child node"
    (are [zipper answer] (= answer (zip/node (zipper->right-child zipper)))
         (zip/vector-zip  [5 10 nil]) nil
         (zip/vector-zip  [5 10 15]) 15
         (zip/vector-zip  ["this" ["is" "big"] "match"]) "match")))

(deftest zipper->value-test
  (testing "Get current node value"
    (are [zipper answer] (= answer (zipper->value zipper))
         (zip/vector-zip [-5 10 nil]) -5
         (zip/vector-zip [[4 9 20] 10 15]) [4 9 20]
         (zip/vector-zip ["this" ["is" "big"] "match"]) "this")))

(deftest tree-empty?-test
  (testing "Find if tree is empty"
    (are [zipper answer] (= answer (tree-empty? zipper))
         (zip/vector-zip ["a" nil nil]) false
         (zip/vector-zip nil) true)))


(deftest replace-node-test
  (testing "Replace current node with given node"
    (are [zipper replacement answer] (= answer (replace-node zipper replacement))
         (zip/vector-zip [50 nil nil]) 100 [100 nil nil]
         (zip/vector-zip [["a" "b" "c"] nil "this"]) "swing" ["swing" nil nil])))

(deftest last-three-chars-test
  (testing "Get last three characters"
    (are [input answer] (= answer (last-three-chars input))
         "abcdf" "cdf"
         "doing" "ing"
         "do" "do"
         "" ""
         "44ing" "ing")))

(deftest valid-elements-test
  (testing "Get valid data for preorder"
    (are [input answer] (= answer (valid-elements input))
         "abcdf" []
         "doing" ["doing"]
         "do" []
         "giving" ["giving"]
         100 [100])))

(deftest pre-order-zipper-test
  (testing "Return sequence with pre order traversal for zipper"
    (are [tree answer] (= answer (pre-order-zipper tree))
         (zip/vector-zip nil) nil
         (zip/vector-zip [50 nil nil]) '(50)
         (zip/vector-zip ["aing" ["f" ["g" nil nil] ["soing" nil nil]] ["d" ["z" nil nil] nil]]) '("aing" "soing")
         (zip/vector-zip [45 [50 [65 [81 nil nil] [72 nil nil]] [82 [95 nil nil] nil]] [87 [90 [99 nil nil] nil] [96 nil nil]]]) '(45 50 65 81 72 82 95 87 90 99 96))))


(deftest pre-order-test
  (testing "Return sequence with pre order traversal"
    (are [tree answer] (= answer (pre-order tree))
         nil nil
         [50 nil nil] '(50)
         ["aing" ["f" ["g" nil nil] ["soing" nil nil]] ["d" ["z" nil nil] nil]] '("aing" "soing")
         [45 [50 [65 [81 nil nil] [72 nil nil]] [82 [95 nil nil] nil]] [87 [90 [99 nil nil] nil] [96 nil nil]]] '(45 50 65 81 72 82 95 87 90 99 96))))


(run-tests)




