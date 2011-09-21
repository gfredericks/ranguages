(ns ranguages.draw
  (:require [clojure.string :as string])
  (:require [ranguages.core :as rc]))

(defn- draw-state-machine
  [states transitions]
  (format
    "digraph G{
      overlap=scale;
      splines=true;
      %s
      %s
    }"
    (string/join
      "\n"
      (for [{:keys [label shape bgcolor]} states]
        (format "%s [label=\"\",shape=%s,style=filled,fillcolor=\"%s\"];"
                label
                shape
                bgcolor)))
    (string/join
      "\n"
      (for [{:keys [from to label]} transitions]
        (format "%s -> %s [label=%s];"
                (name from)
                (name to)
                label)))))

(defn draw-dfa
  [dfa]
  (let [transitions
          (mapcat
            (fn [state]
              (for [[chars state*] (-> dfa :transition state)]
                {:from  state,
                 :to    state*,
                 :label (apply str chars)}))
            (:states dfa)),
        states
          (for [state (:states dfa)]
            {:label (name state),
             :shape
               (if ((:accept dfa) state)
                 "doublecircle"
                 "circle"),
             :bgcolor
               (if (= state (:start dfa))
                 "#6666FF"
                 "white")})]
    (draw-state-machine states transitions)))
