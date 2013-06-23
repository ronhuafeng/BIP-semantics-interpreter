(ns semantics-interpreter.structures.Port
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible))

(defrecord Port
  [type name export? var-list values])

(defn create-port
  ([name export?]
   (->Port 'Port name export? [] (atom [])))
  ([name export? var-list]
   (->Port 'Port name export? var-list (atom []))))

;; TODO: Token and retrieve-port is incompatible.

(extend-type Port

  Accessible

  (project-value
    [this v-map]
    (reduce into {}
                 (map (fn [attr]
                        {attr (attr v-map)})
                   (:var-list this))))

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


  (equal-name?
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


