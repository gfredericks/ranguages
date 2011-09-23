(ns ranguages.counting
  (:require [ranguages.core :as rc]))

(defn- dot-product
  [v1 v2]
  (apply + (map * v1 v2)))

(defn- vecs
  [lazy-matrix]
  (vec (map vec lazy-matrix)))

(defn- matrix
  [dim f]
  (vecs
    (for [i (range dim)]
      (for [j (range dim)]
        (f i j)))))

(let [row get,
      column (fn [m i] (map #(nth % i) m))]
  (defn- matrix-multiply
    [m1 m2]
    (matrix (count m1) (fn [i j] (dot-product (row m1 i) (column m2 j))))))

(defn- identity-matrix
  [dim]
  (matrix dim #(if (= %1 %2) 1 0)))

(defn- matrix-power
  "Repeated squaring algorithm."
  [m p]
  (loop [p p, res (identity-matrix (count m)), mult m]
    (if (zero? p)
      res
      (recur
        (bit-shift-right p 1)
        (if (odd? p)
          (matrix-multiply res mult)
          res)
        (matrix-multiply mult mult)))))

(defn- matching-string-count
  [dfa length]
  ; states have to be numbered starting with the start state
  (let [states (vec (cons (:start dfa) (disj (:states dfa) (:start dfa)))),
        transfer-matrix
          (matrix (count states)
            (fn [i j]
              (let [t (-> dfa :transition (get (states i))),
                    state-j (states j)]
                (reduce
                  (fn [c [chars st]]
                    (if (= st state-j)
                      (+ c (count chars))
                      c))
                  0
                  t)))),
        res-matrix-row (first (matrix-power transfer-matrix length))
        accepting-indices (filter #((:accept dfa) (states %)) (range (count states)))]
    (reduce + (map res-matrix-row accepting-indices))))

; TODO: Optimize with the modified algorithm
(defn- matching-string-count-below
  "Actually this is less-than-or-equal. Should rename."
  [dfa length]
  (apply + (for [i (range (inc length))] (matching-string-count dfa i))))

(defn- suffix-dfa
  "Returns a dfa matching strings that are valid
  suffixes of the given char. All it does is move the starting state forward."
  [{:keys [start transition] :as dfa} char]
  (let [char-set (first (filter #(% char) (-> transition (get start) (keys))))]
    (rc/minimize-dfa
      (assoc dfa :start (-> transition (get start) (get char-set))))))

(defn- sort-char-sets
  "Takes a list of sets of characters and returns a list of lists of
  characters, both in canonical order."
  [char-sets]
  (sort-by
    first
    (map #(sort-by int %) char-sets)))

(defn- matching-string-at-index*
  [{:keys [transition accept start] :as dfa} length i]
  (if (= length 0)
    (if (and (= i 0) (accept start))
      "")
    (let [char-sets (-> transition start keys sort-char-sets)]
      (loop [[char-set & char-sets] char-sets, i i]
        (let [chars (sort-by int char-set),
              dfa* (suffix-dfa dfa (first char-set)),
              string-count (matching-string-count dfa* (dec length)),
              total-count (* string-count (count char-set))]
          (if (>= i total-count)
            (recur char-sets (- i total-count))
            (let [char-i (quot i string-count)]
              (if (= length 1)
                (str (nth chars char-i))
                (str (nth chars char-i)
                     (matching-string-at-index* dfa* (dec length) (rem i string-count)))))))))))

; TODO: Optimize the 2-arg part with squaring
(defn matching-string-at-index
  "Indexed from 0."
  [rang i]
  (let [dfa (rc/minimize-dfa (rc/to-dfa rang))]
    (loop [length 0, i i]
      (let [card (matching-string-count dfa length)]
        (if (< i card)
          (matching-string-at-index* dfa length i)
          (recur (inc length) (- i card)))))))

(defn- index-of-matching-string*
  [{:keys [transition start] :as dfa} s]
  (if (empty? s)
    (if (rc/contains? dfa s) 0)
    (let [char-sets (-> transition start keys sort-char-sets),
          char (first s),
          [preceding-char-sets [relevant-char-set & _]] (split-with #(not ((set %) char)) char-sets),
          preceding-count
            (apply +
              (for [pcs preceding-char-sets]
                (* (count pcs) (matching-string-count (suffix-dfa dfa (first pcs)) (dec (count s)))))),
          dfa* (suffix-dfa dfa (first relevant-char-set)),
          dfa*-count (matching-string-count dfa* (dec (count s))),
          chars-on-same-transition-count (* dfa*-count (count (take-while #(not= % char) relevant-char-set)))]
      (+ preceding-count
         chars-on-same-transition-count
         (index-of-matching-string* dfa* (.substring s 1))))))

(defn index-of-matching-string
  [rang s]
  (let [dfa (rc/minimize-dfa (rc/to-dfa rang))]
    (if (empty? s)
      (if (rc/contains? dfa s) 0)
      (let [shorter-strings-count (matching-string-count-below dfa (dec (count s)))]
        (+ shorter-strings-count (index-of-matching-string* dfa s))))))
