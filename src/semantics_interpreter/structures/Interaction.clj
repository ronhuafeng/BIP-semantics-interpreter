(ns semantics-interpreter.structures.Interaction
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable))

(defrecord Interaction
  [type name
   port connections ;; connection {:component :port }
   time])



(defn create-interaction
  [name port connections time]
  (->Interaction 'Interaction name port connections time))

(defn- all-enable?
  [connections]
  (apply
    (every-pred (fn [c]
                  (enable?
                    (:component c)
                    (:port c))))
    connections))

(defn- fire-interaction!
  [t token]
  (doseq [c (:connections t)]
    (assign-port!
      (:component c)
      (:port c)
      token)))

(extend-type Interaction

  Queryable

  (enable?
    ([this]
     (if (not (nil? (:port this)))
       false
       (all-enable? (:connections this)) #_ ("The guard part is ignored.")
       ))
    ([this port]
     (if (or
           (nil? port) ;; if port is nil or internal port return false.
                       (not (export? port)))
       false
       (all-enable? (:connections this)) #_ ("The guard part is ignored.")
       )))

  Accessible

  (retrieve-port
    [this port]
    (if (all-enable? (:connections this))
      [{:ePort '()}]
      []))
  (assign-port!
    [this port token]
    (fire-interaction! this token))

  Fireable

  (fire!
    [this]
    (fire-interaction! this {:msg "hello"}))
  )


