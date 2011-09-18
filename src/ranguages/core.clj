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
  (union [d1 d2] (doh!))
  (intersection [d1 d2] (doh!))
  (concatenation [d1 d2] (doh!))
  (difference [d1 d2] (doh!)))

(defn- prefix-keyword
  [prefix k]
  (keyword (str (name prefix) (name k))))

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
    (let [trans
            (fn [state c]
              (let [inner (transition state),
                    charset (first (filter #(% c) (keys inner)))]
                (inner charset))),
          epsilon-closure
            (fn [state]
              (loop [sts #{state}]
                (let [sts* (sets/union sts (set (mapcat #(trans % epsilon) sts)))]
                  (if (= sts sts*) sts (recur sts*)))))]
      (not
        (empty?
          (sets/intersection
            accept
            (reduce
              (fn [sts c]
                (set
                  (for [st sts,
                        new-st (trans st c),
                        clsd-st (epsilon-closure new-st)]
                    clsd-st)))
              (epsilon-closure start)
              s))))))
  (to-dfa [n] (doh!))
  (to-re [n] (doh!))
  (to-nfa [n] n)
  (reverse [n] (doh!))
  (star [n] (doh!))
  (union [n1 n2] (doh!))
  (intersection [n1 n2] (doh!))
  (concatenation [n1 n2] (doh!))
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
  [{:keys [states alphabet transition start accept] :as nfa} state]
  {:pre [(not (states state))]}
  (new NFA
       (conj states state)
       alphabet
       (assoc transition
              state
              {(conj alphabet epsilon) #{}})
       start
       accept))

(defn- unify-transition-functions
  "Given two maps from character-sets to state-sets, returns a single
  map that unifies them (i.e., combines the transitions. Whatever.)"
  [tf1 tf2]
  (reduce
    (fn [trans [chars to-states]]
      (reduce
        (fn [trans [chars* state-set]]
          (if (empty? (sets/intersection chars* chars))
            trans
            (-> trans
                (dissoc chars*)
                (assoc
                  (sets/difference chars* chars)
                  state-set)
                (assoc
                  (sets/intersection chars chars*)
                  (sets/union to-states state-set))
                (assoc
                  (sets/difference chars chars*)
                  to-states)
                ; in case any of the preceding assoc's were empty
                (dissoc #{}))))
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
                (if-let [old-chars (old-val chars)]
                  (sets/union (old-chars to-states))
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
  {:pre [(= (:alphabet outer-machine) (:alphabet inner-machine))]}
  (let [outer-machine (prefix-state-names outer-machine :a),
        inner-machine (prefix-state-names inner-machine :b),
        outer-state (prefix-keyword :a outer-state),
        om-states (:states outer-machine),
        im-states (:states inner-machine),
        new-states (-> om-states (disj outer-state) (sets/union im-states))]
    (new NFA
         new-states
         (:alphabet outer-machine)
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
                 (if (-> inner-machine :accepting state)
                   (unify-transition-functions
                     im-trans
                     (-> outer-machine :transition outer-state))
                   im-trans)))))
         (if (= (:start outer-machine) outer-state)
           (:start inner-machine)
           (:start outer-machine))
         (let [out-accept (:accepting outer-machine)]
           (if (out-accept outer-state)
             (->
               out-accept
               (disj outer-state)
               (sets/union (:accepting inner-machine)))
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
  (to-dfa [r] (doh!))
  (to-re [r] r)
  (to-nfa [r]
    (cond
      (set? regex-parse-tree)
        (let [alphabet (conj alphabet ::epsilon)]
          (-> (empty-nfa :a)
            (add-state :b)
            (add-transition :a regex-parse-tree #{:b})))
      (= :star (first regex-parse-tree))
        (let [child-range (to-nfa (first regex-parse-tree))]
          (doh!))))
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
      (new Regex
           alphabet
           (if (set? x)
             x
             (cons (first x) (map parse-regex (rest x))))))
    (rrp/parse-regex alphabet (str re))))

(defn optimize-dfa
  [dfa]
  (doh!))
