(ns semantics-interpreter.structures.Transition
  (use semantics-interpreter.protocols.Queryable))

(defrecord Transition
  [type name source target port])

(defn create-transition
  [name source target port]
  (->Transition 'Transition name source target port))

(extend-type Transition
  Queryable

  (enable? [this place port]
    (and
      (equal-name? place (:source this))
      (equal-name? port (:port this))))
  )

