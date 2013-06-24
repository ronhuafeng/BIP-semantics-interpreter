(ns semantics-interpreter.structures.Interaction
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.structures.Token))

(defrecord Interaction
  [type name
   port connections ;; connection {:component :port}
   time action!])

;; Demo of action!
;; connections
;; 1 and 2 are the indexes of connection in connections
;; :x1 1 :x
;; :x2 2 :y
;; up: tokens -> token
;; down: token -> tokens
#_ (fn action!
     [I direction]
     (cond
       (= direction 'up)
       (create-token
         {:x1 (get-variable I 0 :x )
          :x2 (get-variable I 1 :x )}
         (get-time I))

       (= direction 'down)
       (fn [token]
         ;; v is result of up-action
         (let [v (:value token)]
           {p1 (create-token
                 {:x (+ (:x v) (:y v))}
                 (+ (:time I) (:time token)))
            p2 (create-token
                 {:y (+ (:x v) (:y v))}
                 (+ (:time I) (:time token)))}))))



(defn create-interaction
  ([name port connections time]
   (->Interaction 'Interaction name port connections time
                               (fn [I direction]
                                 (cond
                                   (= direction 'up) {:value {} :time 0}
                                                     (= direction 'down) (fn [token] {})))))
  ([name port connections time action!]
   (->Interaction 'Interaction name port connections time action!)))

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
  (let [up-token (create-token
                   (into
                     (:value ((:action! t) t 'up))
                     (:value token))
                   (:time token))
        value (:value up-token)
        down-tokens (((:action! t) t 'down) up-token)
        time-token (+ (:time up-token)
                     (:time t))]
    ;;TODO: move time addition to 'action!' function
    (doseq [c (:connections t)]
      (assign-port!
        (:component c)
        (:port c)
        ;; generate a proper token for each port
        (if (contains? down-tokens (:port c))
          (assoc
            (get down-tokens (:port c))
            :time time-token)
          (create-token
            {}
            time-token))))))

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

  (get-time
    ([this]
     (apply max
       (map #(get-time (:component %) (:port %))
         (:connections this))))
    ([this port]
     (get-time this)))

  (get-variable
    [this index attr]
    (let [connection (get (:connections this) index)
          c (:component connection)
          p (:port connection)]
      (get-variable c p attr)))
  (retrieve-port
    [this port]
    (if (all-enable? (:connections this))
      ;; only use the up-action result of the values of ports
      [(create-token
         (project-value
           port
           (:value ((:action! this) this 'up)))
         (get-time this))]
      []))
  (assign-port!
    [this port token]
    (fire-interaction! this token))

  Fireable

  (fire!
    [this]
    (fire-interaction!
      this
      (create-token
        {}
        (get-time this))))
  )


