#!/bin/bash lein-exec
# -*- mode: clojure -*-

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

(defn success [result rest]
  {:input  rest
   :output result})

;; Literal/symbol tokens

(defn literal [[firstval & rest]]
  (when (some #(% firstval) [number? keyword? string? char?
                          #(= (type %) java.util.regex.Pattern)])
    (success firstval rest)))

(defn symbol [[firstval & rest]]
  (when (symbol? firstval)
    (success (*known-constants* firstval firstval)
             rest)))

;; Operator tokens

(defn pow [[firstval & rest]]
  (when (= '** firstval)
    (success 'math/expt rest)))

(defn mul [[firstval & rest]]
  (when (= '* firstval)
    (success '* rest)))

(defn div [[firstval & rest]]
  (when (= '/ firstval)
    (success '/ rest)))

(defn add [[firstval & rest]]
  (when (= '+ firstval)
    (success '+ rest)))

(defn sub [[firstval & rest]]
  (when (= '- firstval)
    (success '- rest)))

;; Combinators

(defn sequence
  "Parses an A followed by a B (followed by a ...).
   Succeeds if all parsers succeed, and returns all the outputs concatenated together"
  [& parsers]
  (fn [inp]
    (loop [in              inp
           [parser & more] parsers
           result          []]
      (if-not parser
        (success result in)
        (when-let [{:keys [input output]} (parser in)]
          (recur input
                 more
                 (conj result output)))))))

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
     (fn [in]
       (or (parser in)
           (success default in)))))

(defn repetition
  "Parse a series of at least min 'A's.
   Returns a sequence of min or more A outputs, or nil"
  ([parser]
     (repetition 0 parser))
  ([min parser]
     (fn [inp]
       (loop [in     inp
              result []]
         (if-let [{:keys [input output]} (parser in)]
           (recur input
                  (conj result output))
           (when (>= (count result) min)
             (success result in)))))))

(defn action
  "Apply a function to the result of another parser."
  [func parser]
  (fn [in]
    (when-let [{:keys [input output]} (parser in)]
      (success (func output) input))))

;; Actions

(defn collapse-signs
  "Collapse a series of unary +/- signs into zero or one - signs"
  [[signs expr]]
  (if (odd? (count (filter (partial = '-) signs)))
    (list '- expr)
    expr))

(defn right-assoc
  [[firstval [[operator secondval] & more]]]
  (if secondval
    (list operator firstval (right-assoc [secondval more]))
    firstval))

(defn left-assoc
  [[firstval [[operator secondval] & more]]]
  (if secondval
    (recur [(list operator firstval secondval) more])
    firstval))

(defn left-assoc-collapse
  [[firstval [[operator secondval] & more]]]
  (if secondval
    (let [[same rest] (split-with #(= operator (first %)) more)]
      (recur [(apply list operator firstval secondval (map second same))
              rest]))
    firstval))

(defn apply-function
  [[func args]]
  (cons (*known-functions* func func)
        args))

;; Grammar

(declare expression)

(defn paren
  "A parenthesised sequence of zero or more expressions"
  [[firstval & rest]]
  (when (list? firstval)
    (let [{:keys [input output]} ((repetition expression) firstval)]
      ;; TODO: (assert (not (seq input)))
      (success output rest))))

(defn parenthesised
  "A single parenthesised expression"
  [in]
  (when-let [{:keys [input output]} (paren in)]
    (when (= 1 (count output))
      (success (first output) input))))

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
  (action left-assoc-collapse
          (sequence factor
                    (repetition (sequence (choice mul div) factor)))))

(def expression
  (action left-assoc-collapse
          (sequence term
                    (repetition (sequence (choice add sub) term)))))

;; Parser

(defn parse [s & [parser]]
  (apply prn "Parsing:" s)
  (if-let [{:keys [input output]} ((or parser expression) s)]
    (do (when (seq input)
          (prn "Oops:" input)) ;; TODO: Make this an error
        (prn "Result:" output)
        output)
    (prn "Fail")))

;; Macro

(defmacro $ [& tokens]
  (parse tokens))

;; Tests/examples

(parse '(1 + 1 ** 1 * 1))
(println)

(defn sin [x] (Math/sin x))

(prn "A1:" ($ sin(2 * pi / 3) ))
(prn "A2:" #infix/$ (sin(2 * pi / 3)))
(println)

(defn foo [& m]
  (clojure.string/join "," m))

(prn "B:" ($ foo((2 + 1.9) / 25, 3 ** (1 / 0.7), 1 + - + -1)))
(println)

(def a 6)
(def b 23)
(def c 2)
(prn "C:" ($ (- b + sqrt(b ** 2 - 4 * a * c)) / (2 * a) )
    #in/fix[ (- b - sqrt(b ** 2 - 4 * a * c)) / (2 * a) ])
(println)

(prn "D:" ($ 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 - 2 - 4 - 6 - 8 - 10))

;; TODO:
;; - check whole input is consumed
;; - better error handling
;; - more operators: bitwise/logical/other?
;; - handle missing spaces (where possible): 3 *2 or -a * 4 (but 3*2 can't work)
;; - literal vectors/sets/maps?
;; - member functions: a.foo(b c) => (.foo a b c)
;; - parse strings, e.g.: #in/fix "2*sin(x/7)**2"
;; - maybe precompute constants? (10 * 10 / a) => (/ (* 10 10) a) => (/ 100 a)
;; - numeric-tower version of cbrt (and general nth root?)
