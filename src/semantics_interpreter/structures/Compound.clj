(ns semantics-interpreter.structures.Compound
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (use semantics-interpreter.protocols.Fireable))

(defrecord Compound
  [type name subcomponents exports])

;; exports >> {:target :source :component}

(defn create-compound
  [name subcomponents exports]
  (->Compound 'Compound name subcomponents exports))

(defn- some-enable?
  [components]
  (some
    #(enable? %)
    components))

(defn- enabled-subcomponents
  [compound]
  (filter
    #(enable? %)
    (:subcomponents compound)))

(defn- get-export
  [compound port]
  (some
    #(if (= port (:target %)) %)
    (:exports compound)))


(extend-type Compound
  Queryable

  (enable?
    ([this]
     (some-enable?
       (:subcomponents this)))
    ([this port]
     (let [export (get-export this port)]
       (enable?
         (:source-component export)
         (:source export)))))

  Accessible

  (assign-port!
    [this port token]
    (let [export (get-export this port)]
      (assign-port!
        (:source-component export)
        (:source export)
        token)))
  (retrieve-port
    [this port]
    (let [export (get-export this port)]
      (retrieve-port
        (:source-component export)
        (:source export))))

  (get-time
    ([this]
     (apply min
       (map get-time
         (filter enable?
           (:subcomponents this)))))
    ([this port]
     (let [e (get-export this port)]
       (get-time (:component e) (:source e)))))

  (set-time
    ([this port new-value]
     (let [e (get-export this port)]
       (set-time (:component e) (:source e)))))

  (top-priority
    [this rules selections]
    {:pre [(pos? (count selections))
           "The count of enabled components is more than one."]}
    (let [min-time (apply min
                     (map get-time selections))]
      (rand-nth (filter
                  #(= min-time (get-time %))
                  selections))))

  Fireable

  (fire!
    [this]
    (let [c (top-priority
              this
              []
              (enabled-subcomponents this))]
      (fire! c))))
