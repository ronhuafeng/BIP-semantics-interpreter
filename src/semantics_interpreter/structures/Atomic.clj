(ns semantics-interpreter.structures.Atomic
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable))

(defrecord Atomic
  [type name
   ports places transitions
   time
   variables])
;; variable: {:x v-x :y v-y}

(defn current-place
  [component]
  (first
    (filter enable?
      (:places component))))

(defn current-transitions
  [component]
  (let [current (current-place component)]
    (filter
      #(= current (:source %))
      (:transitions component))))

(defn- add-port-values
  [component]
  (doseq [t (current-transitions component)]
    (add-value!
      (:port t)
      (into
        (project-value
          (:port t)
          (deref (:variables component)))
        {:time (get-time component)}))))
(defn- clear-port-values
  [component]
  (doseq [t (current-transitions component)]
    (clear! (:port t))))

(defn create-atomic
  ([name ports places init transitions time]
   (let [c (->Atomic 'Atomic name
                             ports
                             places
                             transitions
                             (atom time)
                             (atom {}))]
     (do
       (doseq [s (:places c)]
         (clear! s))

       (enable! init)

       (add-port-values c))
     c))
  ([name ports places init transitions time variables]
   (let [c (->Atomic 'Atomic name
                             ports
                             places
                             transitions
                             (atom time)
                             (atom variables))]
     (do
       (doseq [s (:places c)]
         (clear! s))

       (enable! init)

       (add-port-values c))
     c)))



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
    (clear-port-values component)

    (clear! (:source t))

    ;; action stuff
    ((:action! t) component)

    ;;some time stuff
    (set-time
      component
      (+
        (:time t)
        (get-time component)))

    (enable! (:target t))

    (add-port-values component)))

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

  (get-variable
    [this attr]
    (attr (deref (:variables this))))
  (set-variable
    ([this attr new-value]
     (swap! (:variables this)
       assoc
       attr new-value)))

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
          ;; get the transition to fire.
          t (some
              #(if (enable? % current port) %)
              (:transitions this))]
      (do
        (if (nil? (:time token))
          ()
          (set-time this (:time token)))

        (doseq [attr (keys (:value token))]
          (if (contains?
                (deref (:variables this))
                attr)
            (set-variable
              this
              attr
              (get (:value token) attr))))

        (fire-transition this t)))))

