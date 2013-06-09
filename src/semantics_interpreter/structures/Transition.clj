(ns semantics-interpreter.structures.Transition
  (use semantics-interpreter.protocols.Queryable))

(defrecord Transition
    [type name source target label])

(defn create-transition
  [name source target label]
  (->Transition 'Transition name source target label))



