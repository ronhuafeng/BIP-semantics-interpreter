(ns semantics-interpreter.structures.Place
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible))

(defrecord Place
  [type name active?])

(defn create-place
  [name]
  (->Place 'Place name (atom false)))

(extend-type Place
  Queryable
  (equal-name?
    [this that]
    (and (= 'Place
           (:type this)
           (:type that))
      (= (:name this)
        (:name that))))
  (enable? [this]
    (deref (:active? this)))

  Accessible
  (clear! [this]
    (compare-and-set! (:active? this)
      (deref (:active? this))
      false))
  (enable! [this]
    (compare-and-set! (:active? this)
      (deref (:active? this))
      true)))

