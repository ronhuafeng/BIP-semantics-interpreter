(ns semantics-interpreter.structures.Interaction
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable))

(defrecord Interaction
  [type name
   port connections ;; connection {:component :port }
   time action!])

;;
#_ (fn action!
     [direction]
     (cond
       (= direction 'up)
       {:x (:x (first (retrieve-port c1 p1)))
        :y (:y (first (retrieve-port c2 p2)))}

       (= direction 'down)
       (fn [v]
         ;; v is result of up-action
         {p1 {:x (+ (:x v) (:y v))}
          p2 {:y (+ (:x v) (:y v))}})))



(defn create-interaction
  ([name port connections time]
   (->Interaction 'Interaction name port connections time
                               (fn [direction]
                                 (cond
                                   (= direction 'up) {}
                                                     (= direction 'down) (fn [v] {})))))
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
  (let [up-result ((:action! t) 'up)
        values (into
                 up-result
                 (:value token))
        down-result (((:action! t) 'down) values)]
    (doseq [c (:connections t)]
      (assign-port!
        (:component c)
        (:port c)
        ;; generate a proper token for each port
        (assoc
          {:value (get down-result (:port c))}
          :time (+ (:time token)
                  (:time t)))))))

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

  (retrieve-port
    [this port]
    (if (all-enable? (:connections this))
      ;; only use the up-action result of the values of ports
      [(into
         (project-value
           port
           ((:action! this) 'up))
         {:time (get-time this)})]
      []))
  (assign-port!
    [this port token]
    (fire-interaction! this token))

  Fireable

  (fire!
    [this]
    (fire-interaction! this {:time (get-time this)}))
  )


