(ns ranguages.test.counting
  (:use clojure.test
        ranguages.counting)
  (:require [ranguages.core :as rc]))

(deftest index-test
  (let [rang (rc/parse-regex (set "xyz") "x(y|zz)+x*")]
    (doseq [num (range 20)]
      (->> num (matching-string-at-index rang) (index-of-matching-string rang) (= num) (is)))
    (doseq [string ["xy" "xzz" "xzzxxxxx" "xzzyzzyyyx"]]
      (->> string (index-of-matching-string rang) (matching-string-at-index rang) (= string) (is)))))
