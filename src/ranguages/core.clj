(ns ranguages.core)

(defprotocol Ranguage
  []
  (to-dfa        [r] "converts to dfa")
  (to-re         [r] "converts to a regular expression")
  (to-nfa        [r] "converts to an nfa")
  (union         [r other])
  (intersection  [r other])
  (concatenation [r other])
  (difference    [r other])
  (reverse       [r])
  (star          [r]))
