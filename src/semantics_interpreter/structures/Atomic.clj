(ns semantics-interpreter.structures.Atomic
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable))

(defrecord Atomic
  [type name
   ports places transitions
   time])

(defn current-transitions
  [component]
  (let [current (current-place component)]
    (filter
      #(= current (:source %))
      (:transitions component))))

(defn create-atomic
  [name ports places init transitions time]
  (let [c (->Atomic 'Atomic name
                            ports
                            places
                            transitions
                            (atom time))]
    (do
      (doseq [s (:places c)]
        (clear! s))

      (enable! init)

      (doseq [t (current-transitions c)]
        (add-value! (:port t) {:time (get-time c)})))
    c))

(defn enabled-internal-transitions
  [component]
  (let [current (current-place component)]
    (filter
      (fn internal-transition-enable?
        [t]
        (let [p (:port t)]
          (and
            (enable? t current p)
            (not (export? p))
            (enable? p))))
      (:transitions component))))



(defn- fire-transition
  [component t]
  (do
    (doseq [t (current-transitions component)]
      (clear! (:port t)))

    (clear! (:source t))
    ;;TODO: some transition stuff
    ;;some time stuff
    (set-time
      component
      (+
        (:time t)
        (get-time component)))

    (enable! (:target t))

    (doseq [t (current-transitions component)]
      (add-value! (:port t) {:time (get-time component)}))))

(extend-type Atomic
  Queryable
  (enable?
    ([this]
     (if (not-empty
           (enabled-internal-transitions this))
       true
       false))
    ([this port] #_ ("Note: may return nil.")
     (and
       (enable? port)
       (export? port)
       (contain-port? this port))))
  (contain-port?
    [this port]
    (if (not-empty
          (filter
            #(= % port)
            (:ports this)))
      true
      false))

  Fireable
  (fire!
    [this]
    (let [tl (enabled-internal-transitions this)
          t (top-priority this nil tl)]
      (fire-transition this t)))

  Accessible
  (get-time
    ([this]
     (deref (:time this)))
    ([this port]
     (get-time this)))
  (set-time
    ([this new-value]
     (compare-and-set!
       (:time this)
       (get-time this)
       new-value))
    ([this port new-value]
     (set-time this new-value)))
  (current-place
    [this]
    (first
      (filter enable?
        (:places this))))
  (retrieve-port
    [this port]
    (if (contain-port? this port)
      (retrieve-port port)
      []))
  (top-priority
    [this rules selections]
    {:pre [(pos? (count selections))
           "The count of enabled transitions is more than one."]}
    (rand-nth selections))
  (assign-port!
    [this port token]
    (let [current (current-place this)
          t (some
              #(if (enable? % current port) %)
              (:transitions this))]
      (do
        ;;set component's time"
        (if (nil? (:time token))
          ()
          (set-time this (:time token)))

        (fire-transition this t)))))

