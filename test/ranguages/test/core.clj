(ns ranguages.test.core
  (:refer-clojure :except [contains? reverse])
  (:use ranguages.core)
  (:use [clojure.test])
  (:import ranguages.core.NFA))

(deftest prefix-state-names-test
  (let [alpha (set "abc"),
        alpha* (conj alpha epsilon),
        nfa (new NFA
                 #{:foo :bar}
                 alpha
                 {:foo {alpha* #{:bar}},
                  :bar {alpha* #{:bar}}}
                 :bar
                 #{:foo :bar}),
        nfa* (prefix-state-names nfa "xx")]
    (is (= #{:xxfoo :xxbar} (:states nfa*)))
    (is (= #{:xxbar}) (-> nfa* :transition :xxbar alpha*))
    (is (= (:states nfa*) (-> nfa* :transition keys set)))))

(deftest NFA-test
  (let [nfa
          (-> (empty-nfa (set "xyz") :a)
            (add-state :b)
            (add-state :c)
            (add-transition :a #{\x} #{:b})
            (add-transition :b #{\y} #{:b})
            (add-transition :b #{\z} #{:c}))]
    (are [s] (not (contains? nfa s))
         "xz" "xyyyz" "zyx" "" "z" "x" "y" "xzy")
    (let [nfa (add-accepting-state nfa :c)]
      (are [s] (contains? nfa s)
           "xz" "xyz" "xyyz" "xyyyyyyyyyyyyyyz")
      (are [s] (not (contains? nfa s))
           "x" "z" "xy" "xyyy" "zyx" ""))))
