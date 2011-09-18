(ns ranguages.core
  (:require [ranguages.regex-parser :as rrp])
  (:refer-clojure :exclude [contains? reverse])
  (:require [clojure.walk :as wk]
            [clojure.set :as sets]))

(def epsilon ::epsilon)

(defprotocol IRanguage
  (contains?     [r s] "returns true if the language contains the string/seq")
  (to-dfa        [r] "converts to dfa")
  (to-re         [r] "converts to a regular expression")
  (to-nfa        [r] "converts to an nfa")
  (reverse       [r])
  (star          [r])
  (union         [r1 r2]
    "Returns the language of all strings in r1 or r2.")
  (intersection  [r1 r2]
    "Returns the language of all strings in r1 and r2.")
  (concatenation [r1 r2]
    "Returns the language of all strings consisting of a member
    of r1 concatenated with a member of r2.")
  (difference    [r1 r2]
    "Returns the language of all strings in r1 but not r2."))

(defprotocol IHasStateNames
  (prefix-state-names [r prefix]))

(defn doh!
  []
  (throw (new java.lang.UnsupportedOperationException)))

; Types:
;   states: set of keywords
;   alphabet: set of anything? (or chars...)
;   transition:
;     map from
;       state-names
;     to
;       map from
;         subsets of the alphabet (such that the keys form a partition of the alphabet)
;       to
;         a state name
;   start: a state name
;   accept: a set of state names
(defrecord DFA [states alphabet transition start accept]
  IRanguage
  (to-dfa [d] d)
  (to-re [d] (doh!))
  (to-nfa [d] (doh!))
  (reverse [d] (doh!))
  (star [d] (doh!))
  (union [d1 d2] (doh!))
  (intersection [d1 d2] (doh!))
  (concatenation [d1 d2] (doh!))
  (difference [d1 d2] (doh!)))

; Types:
;   states: set of keywords
;   alphabet: set of anything? (or chars...)
;   transition:
;     map from
;       state-names
;     to
;       map from
;         subsets of the (alphabet + ::epsilon) (such that the keys form a partition of the alphabet)
;       to
;         sets of state names
;   start: a state name
;   accept: a set of state names
(defrecord NFA [states alphabet transition start accept]
  IRanguage
  (to-dfa [n] (doh!))
  (to-re [n] (doh!))
  (to-nfa [n] n)
  (reverse [n] (doh!))
  (star [n] (doh!))
  (union [n1 n2] (doh!))
  (intersection [n1 n2] (doh!))
  (concatenation [n1 n2] (doh!))
  (difference [n1 n2] (doh!))
  IHasStateNames
  (prefix-state-names [n prefix]
    (let [state-map (zipmap states (for [state states] (keyword (str prefix (name state)))))]
      (new NFA
           (-> state-map vals set)
           alphabet
           (zipmap
             (map state-map states)
             (for [state states]
               (let [inner-transition-map (transition state)]
                 (zipmap (keys inner-transition-map)
                         (map #(set (map state-map %)) (vals inner-transition-map))))))
           (state-map start)
           (set (map state-map accept))))))

(defn inject-state-machine
  "Replaces the state outer-state in the NFA outer-machine
  with the NFA inner-machine."
  [outer-machine inner-machine outer-state]
  )

; Second argument should be one of the following:
;   - a set of literals
;   - [:star <RE>]
;   - [:qmark <RE>]
;   - [:or <RE> ...]
;   - [:concat <RE> ...]
;
(defrecord Regex [alphabet regex-parse-tree]
  IRanguage
  (to-dfa [r] (doh!))
  (to-re [r] r)
  (to-nfa [r]
    (cond
      (set? regex-parse-tree)
        (let [alphabet (conj alphabet ::epsilon)]
          (new NFA
               #{:a :b :c}
               alphabet
               {:a {regex-parse-tree #{:b},
                    (sets/difference alphabet regex-parse-tree) #{}},
                :b {alphabet #{}}},
               :a
               #{:b}))
      (= :star (first regex-parse-tree))
        (let [child-range (to-nfa (first regex-parse-tree))]
          (doh!))))
  (reverse [r] (doh!))
  (star [r] (doh!))
  (union [r1 r2] (doh!))
  (intersection [r1 r2] (doh!))
  (concatenation [r1 r2] (doh!))
  (difference [r1 r2] (doh!)))

(defn parse-regex
  [alphabet re]
  (wk/postwalk
    (fn [x]
      (new Regex
           alphabet
           (if (set? x)
             x
             (cons (first x) (map parse-regex (rest x))))))
    (rrp/parse-regex alphabet (str re))))

(defn optimize-dfa
  [dfa]
  (doh!))
