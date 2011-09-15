(ns ranguages.regex-parser
  (:use name.choi.joshua.fnparse))

(def special-chars
  {;\[ :l-bracket,
   ;\] :r-bracket,
   \( :l-paren,
   \) :r-paren,
   \. :dot,
   \+ :plus,
   \* :star,
   \| :or})

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

(declare parse)

(defn parse-regex
  "Takes a string representation of a regex and an alphabet and returns
  a parsed Regex object."
  [alphabet s]
  (parse (lex alphabet s)))
