(ns semantics-interpreter.structures.Port
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible))

(defrecord Port
  [type name export? var-list tokens])

(defn create-port
  ([name export?]
   (->Port 'Port name export? [] (atom [])))
  ([name export? var-list]
   (->Port 'Port name export? var-list (atom []))))

;; TODO: Token and retrieve-port is incompatible.

(extend-type Port

  Accessible

  (project-value
    ;; value:  {:x 1 :y 2 :z 3}
    ;; result: {:x 1 :y 2}
    [this value]
    (reduce
      into
      {}
      (map
        (fn [attr]
          {attr (attr value)})
        (:var-list this))))

  (add-token!
    [port token]
    (swap! (:tokens port)
      conj
      token))

  (retrieve-port
    [this]
    (deref (:tokens this)))

  (clear!
    [this]
    (compare-and-set! (:tokens this)
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


