(ns ranguages.test.counting
  (:use clojure.test
        ranguages.counting)
  (:require [ranguages.core :as rc]))

(deftest matching-string-count-test
  (are [re k count] (= (matching-string-count (rc/parse-regex (set "xyz") re) k) count)
    "xyz" 3 1
    "xyz" 4 0
    "xy?z" 2 1
    "xy?z" 3 1
    "xy?z" 4 0
    "(.xy+)|(z(xx|yy|zz).*)" 3 6
    "(.xy+)|(z(xx|yy|zz).*)" 4 12
    "(.xy+)|(xx(y|z).*)" 3 4
    "y*" 0 1
    "y*" 1 1
    "y*" 2 1
    "y*" 3 1
    "y*" 3843 1))
