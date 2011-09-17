(ns ranguages.test.regex-parser
  (:use ranguages.regex-parser)
  (:use clojure.test))

(def HEX (set "abcdef"))

(deftest parse-regex-test
  (are [re pt] (= pt (parse-regex HEX re))
       "abc" [:concat #{\a} #{\b} #{\c}],
       "a|b|c" [:or #{\a} #{\b} #{\c}],
       ".*" [:star HEX],
       "(.*ab)?b+|c+"
         [:or [:concat [:qmark [:concat [:star HEX] #{\a} #{\b}]]
                       [:concat #{\b} [:star #{\b}]]]
              [:concat #{\c} [:star #{\c}]]]))
