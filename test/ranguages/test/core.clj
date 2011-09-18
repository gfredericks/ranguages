(ns ranguages.test.core
  (:require [ranguages.core :as rc])
  (:import ranguages.core.NFA)
  (:use [clojure.test]))

(deftest prefix-state-names-test
  (let [alpha (set "abc"),
        alpha* (conj alpha rc/epsilon),
        nfa (new NFA
                 #{:foo :bar}
                 alpha
                 {:foo {alpha* #{:bar}},
                  :bar {alpha* #{:bar}}}
                 :bar
                 #{:foo :bar}),
        nfa* (rc/prefix-state-names nfa "xx")]
    (is (= #{:xxfoo :xxbar} (:states nfa*)))
    (is (= #{:xxbar}) (-> nfa* :transition :xxbar alpha*))
    (is (= (:states nfa*) (-> nfa* :transition keys set)))))
