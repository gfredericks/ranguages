(ns ranguages.test.regex-parser
  (:use ranguages.regex-parser)
  (:use clojure.test))

(deftest parse-regex-test
  (are [alph re pt] (= pt (parse-regex alph re))
       #{\a \b \c} "abc" [:concat #{\a} #{\b} #{\c}]))
