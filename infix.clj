(use  '[leiningen.exec :only [deps]])
(deps '[[org.clojure/math.numeric-tower "0.0.4"]])

(ns infix
  "Infix notation macro, inspired by:
   http://www.goodmath.org/blog/2014/05/04/combinator-parsing-part-1/"
  (:require [clojure.math.numeric-tower :refer :all])
  (:refer-clojure :exclude [symbol sequence]))

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))

(defn number [[first & rest]]
  (when (number? first)
    [rest first]))

(defn symbol [[first & rest]]
  (when (symbol? first)
    [rest first]))

;; Operators

(defn pow [[first & rest]]
  (when (= '** first)
    [rest 'expt]))

(defn mul [[first & rest]]
  (when (= '* first)
    [rest '*]))

(defn div [[first & rest]]
  (when (= '/ first)
    [rest '/]))

(defn add [[first & rest]]
  (when (= '+ first)
    [rest '+]))

(defn sub [[first & rest]]
  (when (= '- first)
    [rest '-]))

;; Combinators

(defn sequence [& parsers]
  #(loop [in %
          [p & more] parsers
          result []]
     ;;(prn "s" in p more result)
     (if-not p
       [in result]
       (when-let [[inp r] (p in)]
         (recur inp
                more
                (conj result r))))))

(defn choice [& parsers]
  (fn [in] (some #(% in) parsers)))

(defn option [default parser]
  #(or (parser %)
       [% default]))

(defn repetition [min parser]
  #(loop [in %
          result []]
     ;;(prn "rep" in)
     (let [rr (parser in)]
       ;;(prn "->" rr)
       (if-let [[inp r] rr]
         (recur inp
                (conj result r))
         (when (>= (count result) min)
           [in result])))))

(defn action [f parser]
  #(when-let [[in r] (parser %)]
     [in (f r)]))

;; Grammar

(declare add-expr)

(defn paren
  "A parenthesised sequence of zero or more expressions"
  [[first & rest]]
  (when (list? first)
    (let [[in r] ((repetition 0 add-expr) first)]
      ;; TODO: (assert (not (seq in)))
      [rest r])))

(defn parenthesised
  "A single parenthesised expression"
  [in]
  (when-let [[in r] (paren in)]
    (when (= 1 (count r))
      [in (first r)])))

(def function
  (action (fn f [[name args]]
            (cons name args))
          (sequence symbol paren)))

(def simple
  (action (fn [[signs num]]
            (if (odd? (count (filter (partial = '-) signs)))
              (list '- num)
              num))
          (sequence (repetition 0 (choice add sub))
                    (choice function symbol number parenthesised))))

(def pow-expr
  (action (fn f [[a [[s b] & c]]]
            (if b
              (list s a (f [b c]))
              a))
          (sequence simple
                    (repetition 0 (sequence pow simple)))))

(def mul-expr
  (action (fn f [[a [[s b] & c]]]
            (if b
              (f [(list s a b) c])
              a))
          (sequence pow-expr
                    (repetition 0 (sequence (choice mul div) pow-expr)))))

(def add-expr
  (action (fn f [[a [[s b] & c]]]
            (if b
              (f [(list s a b) c])
              a))
          (sequence mul-expr
                    (repetition 0 (sequence (choice add sub) mul-expr)))))

;; Parser

(defn parse [s & [parser]]
  (apply prn "Parsing:" s)
  (let [f (or parser add-expr)
        r (f s)]
    (if-let [[rem res] r]
      (do (when (seq rem)
            (prn "Oops:" rem)) ;; TODO: Make this an error
          (prn "Result:" res)
          res)
      (prn "Fail"))))

(defmacro $= [& tokens]
  (parse tokens))

(defn foo [& m]
  (clojure.string/join "," m))

(prn "A:" ($= sin(3.14) ))

(prn "B:" ($= foo((2 + 1.9) / 25, 3 ** (1 / 0.7))))

(def a 6)
(def b 23)
(def c 2)
(prn "C:" ($= (- b + sqrt(b ** 2 - 4 * a * c)) / (2 * a) )
          ($= (- b - sqrt(b ** 2 - 4 * a * c)) / (2 * a) ))

;; TODO:
;; - check whole input is consumed
;; - better error handling
;; - handle missing spaces (where possible): 3 *2 or -a * 4 (but 3*2 can't work)
