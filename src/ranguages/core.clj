(ns ranguages.core
  (:require [ranguages.regex-parser :as rrp])
  (:refer-clojure :exclude [contains? reverse])
  (:require [clojure.walk :as wk]))

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

(defn doh!
  []
  (throw (new java.lang.UnsupportedOperationException)))

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
  (difference [n1 n2] (doh!)))

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
  (to-nfa [r] (doh!))
  (reverse [r] (doh!))
  (star [r] (doh!))
  (union [r1 r2] (doh!))
  (intersection [r1 r2] (doh!))
  (concatenation [r1 r2] (doh!))
  (difference [r1 r2] (doh!)))

(defn parse-regex
  [alphabet re]
  (wk/postwalk
    (fn [x] (new Regex alphabet x))
    (rrp/parse-regex alphabet (str re))))

(defn optimize-dfa
  [dfa]
  (doh!))
