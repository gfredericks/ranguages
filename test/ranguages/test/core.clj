(ns ranguages.test.core
  (:refer-clojure :exclude [contains? reverse])
  (:use ranguages.core)
  (:use [clojure.test])
  (:import ranguages.core.NFA))

(def regex-fixtures
  (let [alph (set "abc")]
    (partition 4
      ; alphabet regex good bad
      [alph "abc" ["abc"] ["" "a" "b"]
       alph "a+b" ["ab" "aab" "aaab"] ["b" "aabb" "abc"]
       alph "(b?ac*)+a" ["aaa" "baccca" "aaababaca"] ["bb" "bacbaccc"]
       alph "(b((b|c)*)ab+)?c" ["c" "babc" "bbcbcbcabbbc"] ["" "bab" "bcbccbb"]])))

(defn test-regex-examples
  "Calls the supplied function with alphabet, regex, pass-boolean, and example str.
  Pass-boolean indicates if the string should be accepted or not."
  [f]
  (doseq [[alphabet regex good bad] regex-fixtures]
    (doseq [[b ss] [[true good] [false bad]]]
      (doseq [s ss]
        (f alphabet regex b s)))))

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

(deftest nfa-builder-test
  (let [nfa
          (->
            (empty-nfa (set "abc") :a)
            (add-state :b)
            (add-transition :a #{\a} #{:b})
            (add-transition :a #{\c} #{:b}))]
    (is (= (-> nfa :transition :a) {#{epsilon \b} #{}, #{\a \c} #{:b}})))
  (let [nfa
          (->
            (empty-nfa (set "abc") :a)
            (add-state :b :c)
            (add-transition :a #{\a} #{:b})
            (add-transition :a #{\a} #{:c}))]
    (is (= (-> nfa :transition :a) {#{epsilon \b \c} #{}, #{\a} #{:b :c}}))))

(deftest regex-and-nfa-and-stuff-test
  (doseq [[re goods bads]
          [["ab*c"
            ["ac" "abc" "abbc" "abbbc"]
            ["a" "" "c" "bc" "ab" "aabc" "abbbbcc"]]
           ["b*"
            ["" "b" "bb" "bbb"]
            ["a" "bbbc"]]]]
    (let [nfa (-> "abc" set (parse-regex re) (to-nfa))]
      (doseq [good goods]
        (is (contains? nfa good)))
      (doseq [bad bads]
        (is (not (contains? nfa bad)))))))

(deftest complex-regex-to-nfa-test
  (is (to-nfa (parse-regex (set "abc") "a(b+cb)((a?c)a)+"))))

(deftest nfa-to-dfa-and-such-test
  (test-regex-examples
    (fn [alph reg passing? s]
      (let [nfa (-> (parse-regex alph reg) (to-nfa)),
            nfa* (remove-epsilon-transitions nfa),
            dfa (to-dfa nfa),
            dfa* (minimize-dfa dfa)]
        (is (= passing? (contains? nfa s)))
        (is (= passing? (contains? nfa* s)))
        (is (= passing? (contains? dfa s)))
        (is (= passing? (contains? dfa* s)))))))


(deftest minimal-dfa-size-test
  (are [re size] (= size (-> (parse-regex (set "xyz") re) (to-nfa) (to-dfa) (minimize-dfa) (:states) (count)))
       "xyz" 5
       "(zxyz|xxyz|yxyz)" 6)
  (let [dfa (-> (parse-regex (set "xyz") "(.xy+)|(xx(y|z).*)") (to-dfa) (minimize-dfa))]
    ; I confirmed this fact by drawing it out manually.
    (is (< (count (:states dfa)) 9))))
