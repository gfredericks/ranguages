(ns ranguages.regex-parser
  (:require [clojure.walk :as wk])
  (:use name.choi.joshua.fnparse))

(def special-chars
  {;\[ :l-bracket,
   ;\] :r-bracket,
   \{ :l-brace,
   \} :r-brace,
   \, :comma
   \( :l-paren,
   \) :r-paren,
   \. :dot,
   \+ :plus,
   \* :star,
   \| :or,
   \? :qmark})

(def digits (set "0123456789"))

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
      (digits c)
        (let [[s1 s2] (split-with digits s)]
          (recur s2 (conj tokens (new Integer (apply str s1)))))
      :else
        (throw (new Exception (str "Wut is that " (pr-str c)))))))

(declare reggie)
(declare expression)

(def literal
  (alt
    (semantics
      (term char?)
      hash-set)
    (constant-semantics (term #{:dot}) :dot)))

(def posint (term #(and (integer? %) (pos? %))))

(def exact-brace-modifier
  (semantics
    (conc (lit :l-brace) posint (lit :r-brace))
    (fn [[_ i]] {:low i, :high i})))

(def open-brace-modifier
  (semantics
    (conc (lit :l-brace) posint (lit :comma) (lit :r-brace))
    (fn [[_ i]] {:low i})))

(def range-brace-modifier
  (semantics
    (conc (lit :l-brace) posint (lit :comma) posint (lit :r-brace))
    (fn [[_ i _ j]]
      {:low i,
       :high j})))

(def brace-modifier
  (alt exact-brace-modifier open-brace-modifier range-brace-modifier))

(def modifier
  (alt (lit :plus) (lit :star) (lit :qmark) brace-modifier))

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
      (cond
        (keyword? mod)
          (list mod fax)
        (map? mod)
          (cons :concat
            (let [low-fax (repeat (:low mod) fax)]
              (if (:high mod)
                (concat
                  low-fax
                  (repeat (- (:high mod) (:low mod)) [:qmark fax]))
                (concat low-fax [[:star fax]]))))
        :else fax))))

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
