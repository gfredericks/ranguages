(ns ranguages.regexes
  (:require [ranguages.protocols :refer [IRanguage]]
            [ranguages.regex-parser :as rrp]))

; Second argument should be one of the following:
;   - a set of literals
;   - [:star <RE>]
;   - [:qmark <RE>]
;   - [:or <RE> ...]
;   - [:concat <RE> ...]
;
(defrecord Regex [alphabet regex-parse-tree]
  IRanguage
  (contains? [r s] (contains? (to-nfa r) s))
  (to-dfa [r] (-> r to-nfa to-dfa))
  (to-re [r] r)
  (to-nfa [r]
    (cond
      (set? regex-parse-tree)
        (-> (empty-nfa alphabet :a)
          (add-state :b)
          (add-transition :a regex-parse-tree #{:b})
          (add-accepting-state :b))
      (= epsilon regex-parse-tree)
        (-> (empty-nfa alphabet :a) (add-accepting-state :a))
      (= :star (first regex-parse-tree))
        (-> regex-parse-tree second to-nfa star)
      (= :qmark (first regex-parse-tree))
        (let [empty-string-language
                (-> (empty-nfa alphabet :a) (add-accepting-state :a))]
          (-> regex-parse-tree second to-nfa (union empty-string-language)))
      (= :or (first regex-parse-tree))
        (->> regex-parse-tree rest (map to-nfa) (reduce union))
      (= :concat (first regex-parse-tree))
        (->> regex-parse-tree rest (map to-nfa) (reduce concatenation))
      :else (throw (new Exception (str "What's wrong here? -- " (pr-str regex-parse-tree))))))
  (reverse [r] (doh!))
  (star [r]
    (new Regex alphabet [:star r]))
  (union [r1 r2]
    (new Regex alphabet [:or r1 r2]))
  (intersection [r1 r2] (doh!))
  (concatenation [r1 r2]
    (new Regex alphabet [:concat r1 r2]))
  (difference [r1 r2] (doh!)))

(defn regex-to-s
  [re]
  (let [rpt (:regex-parse-tree re),
        hd (if (sequential? rpt) (first rpt))]
    (cond
      (= epsilon rpt)
        ""
      (set? rpt)
        (cond
          (empty? rpt)
            (throw (new Exception "No regex for empty language"))
          (= 1 (count rpt))
            (str (first rpt))
          :else
            (str "(" (string/join "|" (seq rpt)) ")"))
      (= :star hd)
        (str "(" (regex-to-s (second rpt)) ")*")
      (= :qmark hd)
        (str "(" (regex-to-s (second rpt)) ")?")
      (= :or hd)
        (str "(" (string/join "|" (map regex-to-s (rest rpt))) ")")
      (= :concat hd)
        (apply str (map regex-to-s (rest rpt)))
      :else
        (throw (new Exception "This is weird.")))))

(defn parse-regex
  [alphabet re]
  (wk/postwalk
    (fn [x]
      (cond
        (set? x) (new Regex alphabet x)
        (= :epsilon x) (new Regex alphabet epsilon)
        (keyword? x) x
        (char? x) x
        (sequential? x) (new Regex alphabet (cons (first x) (rest x)))))
    (rrp/parse-regex alphabet (str re))))
