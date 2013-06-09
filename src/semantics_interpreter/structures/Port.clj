(ns semantics-interpreter.structures.Port
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible))

(defrecord Port
    [type name export? values])

(defn create-port
  [name export?]
  (->Port 'Port name export? (atom [])))


(extend-type Port

  Accessible

  (add-value!
    [port value]
    (swap! (:values port)
           conj
           value))

  (retrieve-port
    [this]
    (deref (:values this)))

  (clear!
    [this]
    (compare-and-set! (:values this)
                      (retrieve-port this)
                      []))

  Queryable

  (export?
    [port]
    (:export? port))


  (equal?
    [this that]
    (and (= 'Port 
            (:type this)
            (:type that))
         (= (:name this)
            (:name that))))
  (enable?
    ([this]
       (> (count (retrieve-port this))
          0))))


