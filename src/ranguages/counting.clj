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

(defn matching-string-count
  [rang length]
  (let [dfa (rc/minimize-dfa (rc/to-dfa rang)),
        ; states have to be numbered starting with the start state
        states (vec (cons (:start dfa) (disj (:states dfa) (:start dfa)))),
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

(defn matching-string-count-below
  [rang length]
  (apply + (for [i (range (inc length))] (matching-string-count rang i))))

(defn- suffix-dfa
  "Returns a dfa (not necessarily minimized) matching strings that are valid
  suffixes of the given char. All it does is move the starting state forward."
  [{:keys [start transition] :as dfa} char]
  (let [char-set (first (filter #(% char) (-> transition (get start) (keys))))]
    (assoc dfa :start (-> transition (get start) (get char-set)))))

(defn matching-string-at-index
  "Indexed from 0."
  ([rang length i]
    (let [{:keys [transition start] :as dfa} (rc/minimize-dfa (rc/to-dfa rang)),
          char-sets (sort-by #(apply min (map int %)) (-> transition start keys))]
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
                     (matching-string-at-index dfa* (dec length) (rem i string-count))))))))))
  ([rang i]
    (loop [length 0, i i]
      (let [card (matching-string-count rang length)]
        (if (< i card)
          (matching-string-at-index rang length i)
          (recur (inc length) (- i card)))))))
