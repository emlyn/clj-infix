#!/bin/bash lein-exec

(use  '[leiningen.exec :only [deps]])
(deps '[[org.clojure/math.numeric-tower "0.0.4"]])

(ns infix
  "Infix notation macro, inspired by:
   http://www.goodmath.org/blog/2014/05/04/combinator-parsing-part-1/"
  (:require [clojure.math.numeric-tower :as math])
  (:refer-clojure :exclude [symbol sequence]))

(def ^:dynamic *known-constants*
  {'e  Math/E
   'pi Math/PI
   'meaning-of-life 42})

(def ^:dynamic *known-functions*
  {'abs   'math/abs
   'gcd   'math/gcd
   'lcm   'math/lcm
   'floor 'math/floor
   'ceil  'math/ceil
   'round 'math/round
   'sqrt  'math/sqrt
   'acos  #(Math/acos %)
   'asin  #(Math/asin %)
   'atan  #(Math/atan %)
   'atan2 #(Math/atan2 %1 %2)
   'cbrt  #(Math/cbrt %)
   'cos   #(Math/cos %)
   'cosh  #(Math/cosh %)
   'exp   #(Math/exp %)
   'ln    #(Math/log %)
   'log   #(/ (Math/log10 %1) (Math/log %2))
   'log10 #(Math/log10 %)
   'sin   #(Math/sin %)
   'sinh  #(Math/sinh %)
   'tan   #(Math/tan %)
   'tanh  #(Math/tanh %)})

;; Literal/symbol tokens

(defn literal [[first & rest]]
  (when (some #(% first) [number? keyword? string? char?
                          #(= (type %) java.util.regex.Pattern)])
    [rest first]))

(defn symbol [[first & rest]]
  (when (symbol? first)
    [rest (*known-constants* first first)]))

;; Operator tokens

(defn pow [[first & rest]]
  (when (= '** first)
    [rest 'math/expt]))

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

(defn sequence
  "Parses an A followed by a B (followed by a ...).
   Succeeds if all parsers succeed, and returns all the outputs concatenated together"
  [& parsers]
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

(defn choice
  "Parses an A or if that fails, a B (or if that fails a ...).
   Returns the output of the first parses to succeed (or nil)"
  [& parsers]
  (fn [in] (some #(% in) parsers)))

(defn option
  "Parses either an A, or successfully parses nothing.
   Returns either the output of A, or the default value with no input consumed"
  ([parser]
     (option nil parser))
  ([default parser]
     #(or (parser %)
          [% default])))

(defn repetition
  "Parse a series of at least min 'A's.
   Returns a sequence of min or more A outputs, or nil"
  ([parser]
     (repetition 0 parser))
  ([min parser]
     #(loop [in %
             result []]
        (if-let [[inp r] (parser in)]
          (recur inp
                 (conj result r))
          (when (>= (count result) min)
            [in result])))))

(defn action
  "Apply a function to the result of another parser."
  [func parser]
  #(when-let [[in r] (parser %)]
     [in (func r)]))

;; Actions

(defn collapse-signs
  "Collapse a series of unary +/- signs into zero or one - signs"
  [[signs expr]]
  (if (odd? (count (filter (partial = '-) signs)))
    (list '- expr)
    expr))

(defn right-assoc
  [[first [[operator second] & more]]]
  (if second
    (list operator first (right-assoc [second more]))
    first))

(defn left-assoc
  [[first [[operator second] & more]]]
  (if second
    (recur [(list operator first second) more])
    first))

(defn apply-function
  [[func args]]
  (cons (*known-functions* func func)
        args))

;; Grammar

(declare expression)

(defn paren
  "A parenthesised sequence of zero or more expressions"
  [[first & rest]]
  (when (list? first)
    (let [[in r] ((repetition expression) first)]
      ;; TODO: (assert (not (seq in)))
      [rest r])))

(defn parenthesised
  "A single parenthesised expression"
  [in]
  (when-let [[in r] (paren in)]
    (when (= 1 (count r))
      [in (first r)])))

(def function
  "Function application (a symbol followed by a parenthesised list of expressions)"
  (action apply-function
          (sequence symbol paren)))

(def simple
  "A single function, symbol, literal or parenthesised expression,
   preceded by zero or more unary + and/or - operators"
  (action collapse-signs
          (sequence (repetition (choice add sub))
                    (choice function symbol literal parenthesised))))

(def factor
  (action right-assoc
          (sequence simple
                    (repetition (sequence pow simple)))))

(def term
  (action left-assoc
          (sequence factor
                    (repetition (sequence (choice mul div) factor)))))

(def expression
  (action left-assoc
          (sequence term
                    (repetition (sequence (choice add sub) term)))))

;; Parser

(defn parse [s & [parser]]
  (apply prn "Parsing:" s)
  (if-let [[rem res] ((or parser expression) s)]
    (do (when (seq rem)
          (prn "Oops:" rem)) ;; TODO: Make this an error
        (prn "Result:" res)
        res)
    (prn "Fail")))

;; Macro

(defmacro $ [& tokens]
  (parse tokens))

;; Tests/examples

(defn sin [x] (Math/sin x))

(prn "A1:" ($ sin(2 * pi / 3) ))
(prn "A2:" #infix/$ (sin(2 * pi / 3)))

(defn foo [& m]
  (clojure.string/join "," m))

(prn "B:" ($ foo((2 + 1.9) / 25, 3 ** (1 / 0.7), 1 + - + -1)))

(def a 6)
(def b 23)
(def c 2)
(prn "C:" ($ (- b + sqrt(b ** 2 - 4 * a * c)) / (2 * a) )
    #in/fix[ (- b - sqrt(b ** 2 - 4 * a * c)) / (2 * a) ])

;; TODO:
;; - check whole input is consumed
;; - better error handling
;; - more operators: bitwise/logical/other?
;; - handle missing spaces (where possible): 3 *2 or -a * 4 (but 3*2 can't work)
;; - literal vectors/sets/maps?
;; - member functions: a.foo(b c) => (.foo a b c)
