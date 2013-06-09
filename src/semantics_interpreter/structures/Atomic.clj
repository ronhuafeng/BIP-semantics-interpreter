(ns semantics-interpreter.structures.Atomic
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable))

(defrecord Atomic
    [type name 
     ports places transitions
     time])

(defn create-atomic
  [name ports places init transitions time]
  (let [c (->Atomic 'Atomic name 
                    (map atom ports)
                    (map atom places)
                    transitions
                    time)]
    (do
      (map (fn [p]
             (if (equal? init (deref p))
               (enable! p) #_ ("set initial place to be enabled.")
               (clear! p))) #_ ("set other places to be disabled.")
           (:places c))
      c)))



(extend-type Atomic
  Queryable
  (enable?
    ([this]
       (let [current (first )]))
    ([this port] false))
  Fireable
  (fire!
    [this])
  Accessible
  (current-place
    [this]
    (first
     (filter (comp enable? deref)
             (:places current))))
  (retrieve-port
    [this port])
  (assign-port!
    [this port token]))

