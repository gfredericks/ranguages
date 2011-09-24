(ns ranguages.regex-parser
  (:require [clojure.walk :as wk])
  (:use name.choi.joshua.fnparse))

(def special-chars
  {;\[ :l-bracket,
   ;\] :r-bracket,
   \( :l-paren,
   \) :r-paren,
   \. :dot,
   \+ :plus,
   \* :star,
   \| :or,
   \? :qmark})

(defn- lex
  [alphabet s]
  (loop [[c & s* :as s] (seq s), tokens []]
    (cond
      (empty? s)
        tokens
      (= c \\)
        (if (alphabet (first s*))
          (recur (next s*) (conj tokens (first s*)))
          (throw (new Exception "Character not in alphabet")))
      (special-chars c)
        (recur s* (conj tokens (special-chars c)))
      (alphabet c)
        (recur s* (conj tokens c))
      :else
        (throw (new Exception "Wut is that")))))

(declare reggie)
(declare expression)

(def literal
  (alt
    (semantics
      (term char?)
      hash-set)
    (constant-semantics (term #{:dot}) :dot)))

(def modifier
  (alt (lit :plus) (lit :star) (lit :qmark)))

(def factor
  (alt
    literal
    (semantics
      (conc (lit :l-paren) expression (lit :r-paren))
      (fn [[_ exp _]]
        exp))))

(def modified-factor
  (semantics
    (conc factor (opt modifier))
    (fn [[fax mod]]
      (if mod
        (list mod fax)
        fax))))

(def tterm
  (semantics
    (rep+ modified-factor)
    (fn [mfs]
      (if (= 1 (count mfs))
        (first mfs)
        (cons :concat mfs)))))

(def expression
  (alt
    (semantics
      (conc tterm (rep* (conc (lit :or) tterm)))
      (fn [[first-term other-terms]]
        (if (empty? other-terms)
          first-term
          (list* :or first-term (map second other-terms)))))
    (constant-semantics emptiness :epsilon)))


(def reggie expression)

(defn parse-regex
  "Takes a string representation of a regex and an alphabet and returns
  a regex parse tree.

  Tree Representation: not sure. Maybe we should use a defrecord to distinguish...?"
  [alphabet s]
  (let [match
          (rule-match
            reggie
            (fn [& _] (throw (new Exception "PARSE FAILURE")))
            (fn [& _] (throw (new Exception "PARSE FAILURE")))
            {:remainder (lex alphabet s)})]
    (wk/postwalk
      #(cond
         (= % :dot) alphabet
         (and (list? %) (= :plus (first %)))
           (list :concat (second %) (list :star (second %)))
         :else %)
      match)))
