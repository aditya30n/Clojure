(ns min-heap.zip
(:require [clojure.zip :as zip] )
  (:gen-class))

(def min-heap nil)

(defn zipper->left-child
  "Find left child of current node"
  [zipper]
  (-> zipper zip/down zip/right))

(defn zipper->right-child
  "Find right child of current node"
  [zipper]
  (-> zipper zip/down zip/rightmost))

(defn zipper->value
  "Get the value at root node"
  [zipper]
  (if (zip/node zipper)
    (-> zipper zip/down zip/node)
    nil))

(defn tree-empty?
  "Find if zipper tree is empty or not"
  [zipper]
  (not (zip/node zipper)))

(defn replace-node
  "Replace current node with new replacement"
  [zipper replacement]
  (let [location (zip/node zipper)
        node (zip/make-node zipper location [replacement nil nil])]
    (-> zipper (zip/replace node) zip/root)))

(defn zipper-height
  "Find height of the zipper tree"
  [zipper x]
  (if (tree-empty? zipper)
    x
    (max (zipper-height (zipper->left-child zipper) (inc x)) (zipper-height (zipper->right-child zipper) (inc x)))))

(defn min-heap-zipper-insert
  "Insert new node into zipper tree"
  [zipper x]
  (let [value (zipper->value zipper)
        compare-value (compare x value)]
    (cond
     (tree-empty? zipper) (replace-node zipper x)
     (> 0 compare-value) (recur (assoc-in zipper [0 0] x) value)
     (<= 0 compare-value) (let [height-left (zipper-height (zipper->left-child zipper) 0)
                                height-right (zipper-height (zipper->right-child zipper) 0)]
                            (cond
                             (> height-left height-right) (recur (zipper->right-child zipper) x)
                             (<= height-left height-right) (recur (zipper->left-child zipper) x))))))

(defn min-heap-insert
  "Insert new node into min heap tree"
  [tree x]
  (if (or (number? x) (string? x))
  (min-heap-zipper-insert (zip/vector-zip tree) x)))

(defn last-three-chars
  "Find last three characters of a string"
  [input]
  (let [length (.length input)]
      (if (>= length 3)
        (subs input (- length 3) length)
        input)))

(defn valid-elements
  "Find if string ends with 'ing'or number"
  [input]
  (if (string? input)
    (if (= 0 (compare (last-three-chars input) "ing"))
      [input]
      [])
    [input]))

(defn pre-order-zipper
  "Returns pre order traversal of zipper"
  [zipper]
  (if-not (tree-empty? zipper)
    (concat (valid-elements (zipper->value zipper)) (pre-order-zipper (zipper->left-child zipper)) (pre-order-zipper (zipper->right-child zipper)))))

(defn pre-order
  "Returns pre order traversal of tree"
  [tree]
    (pre-order-zipper (zip/vector-zip tree)))


