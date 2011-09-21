(ns ranguages.core
  (:require [ranguages.regex-parser :as rrp])
  (:refer-clojure :exclude [contains? reverse])
  (:require [clojure.walk :as wk]
            [clojure.set :as sets]))

(def epsilon ::epsilon)

(defprotocol IRanguage
  (contains?     [r s] "returns true if the language contains the string/seq")
  (to-dfa        [r] "converts to dfa")
  (to-re         [r] "converts to a regular expression")
  (to-nfa        [r] "converts to an nfa")
  (reverse       [r])
  (star          [r])
  (union         [r1 r2]
    "Returns the language of all strings in r1 or r2.")
  (intersection  [r1 r2]
    "Returns the language of all strings in r1 and r2.")
  (concatenation [r1 r2]
    "Returns the language of all strings consisting of a member
    of r1 concatenated with a member of r2.")
  (difference    [r1 r2]
    "Returns the language of all strings in r1 but not r2."))

(defprotocol IHasStateNames
  (prefix-state-names [r prefix]))

(defn- doh!
  []
  (throw (new java.lang.UnsupportedOperationException)))

(declare dfa-cartesian-product)

; Represents a strict DFA, where every state/char maps to an
; existing state (i.e., there are no implicit transitions to
; a dead state).
;
; Types:
;   states: set of keywords
;   alphabet: set of anything? (or chars...)
;   transition:
;     map from
;       state-names
;     to
;       map from
;         subsets of the alphabet (such that the keys form a partition of the alphabet)
;       to
;         a state name
;   start: a state name
;   accept: a set of state names
(defrecord DFA [states alphabet transition start accept]
  IRanguage
  (contains? [d s]
    (boolean
      (accept
        (reduce
          (fn [state c]
            (let [inner (transition state),
                  char-set (first (filter #(% c) (keys inner)))]
              (inner char-set)))
          start
          s))))
  (to-dfa [d] d)
  (to-re [d] (doh!))
  (to-nfa [d] (doh!))
  (reverse [d] (doh!))
  (star [d] (doh!))
  (union [d1 d2]
    (dfa-cartesian-product
      d1
      (to-dfa d2)
      (fn [s1 s2]
        (or ((:accept d1) s1) ((:accept d2) s2)))))
  (intersection [d1 d2]
    (dfa-cartesian-product
      d1
      (to-dfa d2)
      (fn [s1 s2]
        (and ((:accept d1) s1) ((:accept d2) s2)))))
  (concatenation [d1 d2] (doh!))
  (difference [d1 d2]
    (dfa-cartesian-product
      d1
      (to-dfa d2)
      (fn [s1 s2]
        (and ((:accept d1) s1) (not ((:accept d2) s2)))))))

(defn dfa-xor
  [d1 d2]
  (dfa-cartesian-product
    d1
    (to-dfa d2)
    (fn [s1 s2]
      (let [b1 ((:accept d1) s1), b2 ((:accept d2) s2)]
        (or (and b1 (not b2))
            (and b2 (not b1)))))))

(defn- invert-map
  [m]
  (reduce
    (fn [m2 [k v]]
      (update-in m2 [v] conj k))
    {}
    m))

(defn- simplify-trans-map
  "Given a map with charset keys, unifies all keys that map to the
  same value (whether states or state-sets)."
  [m]
  (let [inv (invert-map m)]
    (reduce
      (fn [m [v ks]]
        (if (> (count ks) 1)
          (assoc
            (apply dissoc m ks)
            (apply sets/union ks)
            v)
          m))
      m
      inv)))

(defn- dfa-cartesian-product
  "The accept-fn should take two arguments, the first being
  a state from dfa1 and the second a state from dfa2. It
  should return true if the combined state should be accepting
  and false otherwise."
  [dfa1 dfa2 accept-fn]
  {:pre [(= (:alphabet dfa1) (:alphabet dfa2))]}
  (let [new-states
          (zipmap
            (for [s1 (:states dfa1), s2 (:states dfa2)] [s1 s2])
            (for [s (range)] (keyword (str "s" s))))]
    (new DFA
         (set (vals new-states))
         (:alphabet dfa1)
         (zipmap
           (vals new-states)
           (for [[s1 s2] (keys new-states)]
             ; TODO: simplify the result?
             (->
               (for [[chars1 state1] (-> dfa1 :transition s1),
                     [chars2 state2] (-> dfa2 :transition s2)]
                 {(sets/intersection chars1 chars2)
                    (new-states [state1 state2])})
               (->> (reduce merge))
               (dissoc #{})
               (simplify-trans-map))))
         (new-states [(:start dfa1) (:start dfa2)])
         (set (map new-states (filter (partial apply accept-fn) (keys new-states)))))))

(defn- merge-states
  [{:keys [start states transition accept] :as dfa} a b]
  (if (= start b)
    (merge-states dfa b a)
    (assoc dfa
           :states (disj states b)
           :accept (disj accept b)
           :transition
             (let [transition (dissoc transition b)]
               (zipmap
                 (keys transition)
                 (for [t (vals transition)]
                   (zipmap
                     (keys t)
                     (for [v (vals t)]
                       (if (= v b) a v)))))))))

(defn- transition-dfa
  [{t :transition} state c]
  (let [z (t state),
        charset (first (filter #(% c) (keys z)))]
    (z charset)))

(defn- consolidate-states
  [{:keys [accept alphabet states transition start] :as dfa}]
  (if (empty? accept)
    ; Return a one-state reject-everything DFA
    (new DFA #{start} alphabet {start {alphabet start}} start #{})
    (loop [distinguishable?
             (let [state-pairs (for [a states, b states, :when (< (hash a) (hash b))] [a b])]
               (zipmap
                 (map set state-pairs)
                 (for [[a b] state-pairs] (apply not= (for [s [a b]] (boolean (accept s)))))))]
      (let [distinguishable*
              (reduce
                (fn [dis [a b]]
                  (if (some
                        (fn [c]
                          (let [[s1 s2] (for [s [a b]] (transition-dfa dfa s c))]
                            (and (not= s1 s2) (dis #{s1 s2}))))
                        alphabet)
                    (assoc dis #{a b} true)
                    dis))
                distinguishable?
                (map seq (remove distinguishable? (keys distinguishable?))))]
        (if (= distinguishable? distinguishable*)
          (reduce
            (fn [dfa state]
              (let [buddy (first (filter #(and (> (hash %) (hash state)) (not (distinguishable? #{state %}))) states))]
                (if buddy
                  (merge-states dfa buddy state)
                  dfa)))
            dfa
            (sort-by hash states))
          (recur distinguishable*))))))

(defn minimize-dfa
  [{:keys [start states transition] :as dfa}]
  (let [reachable-states
          (loop [reachables #{},
                 reaching #{start}]
            (if (empty? reaching)
              reachables
              (let [next-state (first reaching),
                    reaches (-> next-state transition vals set)]
                (recur
                  (conj reachables next-state)
                  (->
                    (sets/difference reaches reachables)
                    (sets/union reaching)
                    (disj next-state))))))]
    (consolidate-states
      (assoc dfa
             :states reachable-states
             :transition (select-keys transition reachable-states)))))

(defn- prefix-keyword
  [prefix k]
  (keyword (str (name prefix) (name k))))

(declare add-accepting-state
         empty-nfa
         add-state
         add-transition
         inject-state-machine
         unify-transition-functions)

(defn- nfa-transition
  "Returns the set of states that a particular state and character
  lead to."
  [nfa state c]
  (let [inner ((:transition nfa) state),
        charset (first (filter #(% c) (keys inner)))]
    (inner charset)))

(defn- nfa-epsilon-closure
  [nfa state]
  (loop [sts #{state}]
    (let [sts* (sets/union sts (set (mapcat #(nfa-transition nfa % epsilon) sts)))]
      (if (= sts sts*) sts (recur sts*)))))

(defn remove-epsilon-transitions
  "Returns an equvilant nfa where epsilon never leads anywhere. This
  can be effectively treated as an NFA without epsilon transitions by
  ignoring that part of the transition function."
  [{:keys [states transition start accept] :as nfa}]
  (let [epsilon-closure (memoize (partial nfa-epsilon-closure nfa)),
        ; add start state as accepting if necessary
        start-is-accept?
          (not (empty? (sets/intersection accept (set (epsilon-closure start))))),
        remove-epsilons
          (fn [t]
            (let [t2 (zipmap (for [k (keys t)] (disj k epsilon)) (vals t))]
              (-> t2 (assoc #{epsilon} #{}) (dissoc #{}))))
        new-transition
          (zipmap
            states
            (for [state states]
              (reduce
                unify-transition-functions
                (for [state* (epsilon-closure state)]
                  (let [t (remove-epsilons (transition state*))]
                    (zipmap
                      (keys t)
                      (for [v (vals t)]
                        (reduce sets/union (map epsilon-closure v)))))))))]
    (-> nfa
      (assoc :transition new-transition)
      (assoc :accept (if start-is-accept? (conj accept start) accept)))))

; Represents an NFA with epsilon transitions. Because each
; state/char maps to a subset of states, non-accepting paths
; can be modeled as simply transition to the empty-set of states.
;
; Argument Types:
;   states: set of keywords
;   alphabet: set of anything? (or chars...)
;   transition:
;     map from
;       state-names
;     to
;       map from
;         subsets of the (alphabet + ::epsilon) (such that the keys form a partition of the alphabet)
;       to
;         sets of state names
;   start: a state name
;   accept: a set of state names
(defrecord NFA [states alphabet transition start accept]
  IRanguage
  (contains? [n s]
    (not
      (empty?
        (sets/intersection
          accept
          (reduce
            (fn [sts c]
              (set
                (for [st sts,
                      new-st (nfa-transition n st c),
                      clsd-st (nfa-epsilon-closure n new-st)]
                  clsd-st)))
            (nfa-epsilon-closure n start)
            s)))))
  (to-dfa [n]
    (let [{:keys [transition start] :as nfa} (remove-epsilon-transitions n)]
      (loop [upcoming-states #{#{start}},
             transitions {}]
        (if (empty? upcoming-states)
          (let [state-names (zipmap (keys transitions) (for [x (range (count transitions))] (keyword (str "s" x))))]
            (new DFA
                 (-> state-names vals set)
                 alphabet
                 (zipmap
                   (map state-names (keys transitions))
                   (for [tf (vals transitions)]
                     (zipmap
                       (keys tf)
                       (map state-names (vals tf)))))
                 (state-names #{start})
                 (set
                   (map state-names
                     (filter #(not (empty? (sets/intersection % accept))) (keys state-names))))))

          (let [next-state (first upcoming-states),
                trans-fn
                  (if (empty? next-state)
                    {alphabet #{}}
                    (let [t-with-epsilon
                            (reduce
                              unify-transition-functions
                              (map transition next-state))]
                      (->
                        (zipmap
                          (for [k (keys t-with-epsilon)]
                            (disj k epsilon))
                          (vals t-with-epsilon))
                        (dissoc #{}))))]
            (recur
              (disj
                (sets/union
                  upcoming-states
                  (set (remove (set (keys transitions)) (vals trans-fn))))
                next-state)
              (assoc transitions next-state trans-fn)))))))

  (to-re [n] (doh!))
  (to-nfa [n] n)
  (reverse [n] (doh!))
  (star [n]
    (let [star-skeleton
            (->
              (empty-nfa alphabet :a)
              (add-state :b :c)
              (add-transition :a #{epsilon} #{:b :c})
              (add-transition :b #{epsilon} #{:c})
              (add-transition :c #{epsilon} #{:a})
              (add-accepting-state :c))]
      (inject-state-machine star-skeleton (prefix-state-names n :z) :b)))
  (union [n1 n2]
    (->
      (empty-nfa alphabet :a)
      (add-state :b :c :d)
      (add-transition :a #{epsilon} #{:b :c})
      (add-transition :b #{epsilon} #{:d})
      (add-transition :c #{epsilon} #{:d})
      (add-accepting-state :d)
      (inject-state-machine (prefix-state-names n1 :x) :b)
      (inject-state-machine (prefix-state-names n2 :y) :c)))
  (intersection [n1 n2] (doh!))
  (concatenation [n1 n2]
    (->
      (empty-nfa alphabet :a)
      (add-state :b :c :d)
      (add-transition :a #{epsilon} #{:b})
      (add-transition :b #{epsilon} #{:c})
      (add-transition :c #{epsilon} #{:d})
      (add-accepting-state :d)
      (inject-state-machine (prefix-state-names n1 :x) :b)
      (inject-state-machine (prefix-state-names n2 :y) :c)))
  (difference [n1 n2] (doh!))
  IHasStateNames
  (prefix-state-names [n prefix]
    (let [state-map (zipmap states (map (partial prefix-keyword prefix) states))]
      (new NFA
           (-> state-map vals set)
           alphabet
           (zipmap
             (map state-map states)
             (for [state states]
               (let [inner-transition-map (transition state)]
                 (zipmap (keys inner-transition-map)
                         (map #(set (map state-map %)) (vals inner-transition-map))))))
           (state-map start)
           (set (map state-map accept))))))

; NFA building functions
(defn empty-nfa
  "Creates an NFA of one state, where all characters transition
  to the empty set (i.e., it recognizes the empty language).
  Only really useful for combining with the other NFA builder
  functions."
  [alphabet only-state]
  (new NFA
       #{only-state}
       alphabet
       {only-state {(conj alphabet epsilon) #{}}}
       only-state
       #{}))

(defn remove-state
  [{:keys [states alphabet transition start accept] :as nfa} state]
  {:pre [(not= state start) (states state)]}
  (new NFA
       (disj states state)
       alphabet
       (let [transition (dissoc transition state)]
         (zipmap
           (keys transition)
           (for [inner (vals transition)]
             (zipmap (keys inner) (map #(disj % state) (vals inner))))))
       start
       (disj accept state)))

(defn add-state
  "Adds a new state with no incoming or outgoing transitions."
  ([nfa s1 s2 & ss]
    (reduce add-state nfa (list* s1 s2 ss)))
  ([{:keys [states alphabet transition start accept] :as nfa} state]
  {:pre [(not (states state))]}
  (->
    nfa
    (update-in [:states] conj state)
    (assoc-in [:transition state] {(conj alphabet epsilon) #{}}))))

(defn- unify-transition-functions
  "Given two maps from character-sets to state-sets, returns a single
  map that unifies them (i.e., combines the transitions. Whatever.)"
  [tf1 tf2]
  {:post [(not (nil? %))]}
  (reduce
    (fn [trans [chars to-states]]
      (reduce
        (fn [trans [chars* state-set]]
          (let [new-stuff
                  {(sets/difference chars* chars) state-set,
                   (sets/intersection chars* chars) (sets/union to-states state-set)}]
            (->
              trans
              (dissoc chars*)
              (merge new-stuff)
              (dissoc #{})
              (simplify-trans-map))))
        trans
        trans))
    tf1
    tf2))

(defn add-transition
  [{:keys [states alphabet transition start accept] :as nfa} from-state chars to-states]
  {:pre [(states from-state)
         (sets/subset? to-states states)
         (sets/subset? chars (conj alphabet epsilon))]}
  (new NFA
       states
       alphabet
       (assoc transition
              from-state
              (let [old-val (transition from-state)]
                (if-let [old-chars-states (old-val chars)]
                  (assoc old-val chars (sets/union old-chars-states to-states))
                  (unify-transition-functions old-val {chars to-states}))))
       start
       accept))

(defn add-accepting-state
  [{:keys [states alphabet transition start accept] :as nfa} state]
  {:pre [(states state)]}
  (new NFA states alphabet transition start (conj accept state)))

(defn inject-state-machine
  "Replaces the state outer-state in the NFA outer-machine
  with the NFA inner-machine. Will rename states."
  [outer-machine inner-machine outer-state]
  {:pre [(= (:alphabet outer-machine) (:alphabet inner-machine))
         (empty? (sets/intersection (:states outer-machine) (:states inner-machine)))]
   ; result should not contain the replaced state
   :post [(-> % :states (get outer-state) nil?)]}
  (let [om-states (:states outer-machine),
        im-states (:states inner-machine),
        new-states (-> om-states (disj outer-state) (sets/union im-states)),
        new-transition
          (zipmap
            new-states
            (for [state new-states]
              (if (om-states state)
                (let [om-trans (-> outer-machine :transition state)]
                  (zipmap
                    (keys om-trans)
                    (for [state-set (vals om-trans)]
                      (if (state-set outer-state)
                        (-> state-set (disj outer-state) (conj (:start inner-machine)))
                        state-set))))
                (let [im-trans (-> inner-machine :transition state)]
                  (if (-> inner-machine :accept state)
                    (unify-transition-functions
                      im-trans
                      (-> outer-machine :transition outer-state))
                    im-trans)))))]
    (new NFA
         new-states
         (:alphabet outer-machine)
         new-transition
         (if (= (:start outer-machine) outer-state)
           (:start inner-machine)
           (:start outer-machine))
         (let [out-accept (:accept outer-machine)]
           (if (out-accept outer-state)
             (->
               out-accept
               (disj outer-state)
               (sets/union (:accept inner-machine)))
             out-accept)))))

; Second argument should be one of the following:
;   - a set of literals
;   - [:star <RE>]
;   - [:qmark <RE>]
;   - [:or <RE> ...]
;   - [:concat <RE> ...]
;
(defrecord Regex [alphabet regex-parse-tree]
  IRanguage
  (to-dfa [r] (-> r to-nfa to-dfa))
  (to-re [r] r)
  (to-nfa [r]
    (cond
      (set? regex-parse-tree)
        (-> (empty-nfa alphabet :a)
          (add-state :b)
          (add-transition :a regex-parse-tree #{:b})
          (add-accepting-state :b))
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
  (star [r] (doh!))
  (union [r1 r2] (doh!))
  (intersection [r1 r2] (doh!))
  (concatenation [r1 r2] (doh!))
  (difference [r1 r2] (doh!)))

(defn parse-regex
  [alphabet re]
  (wk/postwalk
    (fn [x]
      (cond
        (set? x) (new Regex alphabet x)
        (keyword? x) x
        (char? x) x
        (sequential? x) (new Regex alphabet (cons (first x) (rest x)))))
    (rrp/parse-regex alphabet (str re))))
