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
