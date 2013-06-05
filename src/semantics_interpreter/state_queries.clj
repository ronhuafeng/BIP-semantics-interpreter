(ns semantics-interpreter.state-queries
  (use semantics-interpreter.data-structure)
  (use semantics-interpreter.dataset))



(defmulti get-current
  "Get the current place which represents the atomic component's state."
  (fn [component] (:type component)))

(defmethod get-current 'atomic
  [component]
  (deref (:current component)))

(defmulti set-current
  "Get the current place which represents the atomic component's state."
  (fn [component place] (:type component)))

(defmethod set-current 'atomic
  [component place]
  (compare-and-set! (:current component)
    (get-current component)
    place))



(defn attrs-equal?
  [p1 p2 & attr-coll]
  "Test if p1 and p2 are equal considering attributes in attr-coll"
  (reduce #(and %1 %2) true
    (map (fn [attr]
           (= (attr p1) (attr p2)))
      attr-coll)))

(defmulti port-enable?
  "Query if a component's port is enabled."
  (fn [component port] (:type component)))

(defmethod port-enable? 'atomic
  [component port]
  (not (empty? (filter (fn [t]
                         (and (attrs-equal? (get-current component) (:from t) :name )
                           (attrs-equal? port (:port t) :name )))
                 (:transitions component)))))
(defmethod port-enable? 'compound
  [component port]
  (not (empty? (filter #(if (attrs-equal? port (:target %) :name )
                          (port-enable? (:source-component %) port)
                          false)
                 (:port-bindings component)))))
(defmethod port-enable? 'interaction
  [component port]
  (if (attrs-equal? port (:port component) :name )
    (reduce #(and %1 %2) true
      (map (fn [t]
             (port-enable? (:source-component t)
               (:source t)))
        (:port-bindings component)))
    false))

(defmulti possible-ports
  "Generate a list containing all possible ports
   (who can participate in a interaction)"
  (fn [component] (:type component)))

(defmethod possible-ports 'atomic
  [component]
  (map (fn [t] (:port t))
    (filter (fn [t]
              (and (= true (:export? (:port t)))
                (= (get-current component) (:from t))))
      (:transitions component))))
;; blood and tear experience: misspell port-bingdings to port-binding
(defmethod possible-ports 'compound
  [component]
  (map :target (filter #(and (:export? (:target %))
                          (port-enable? (:source-component %) (:source %)))
                 (:port-bindings component))))

(defmethod possible-ports 'interaction
  [component]
  "A interaction has a enabled port only if it has a 'export' port
   and all other component related ports is enabled."
  (filter (fn [p]
            (and (:export? p)
              (port-enable? component p)))
    [(:port component)]))

(defmulti component-enable?
  "A enabled component can perform a firing itself without
   refering to its 'export' port"
  (fn [component] (:type component)))

(defmethod component-enable? 'atomic
  [component]
  (not (empty? (filter (fn [t]
                         (and (= (:name (get-current component))
                                (:name (:from t)))
                           (not (:export? (:port t)))))
                 (:transitions component)))))

(defmethod component-enable? 'compound
  [component]
  (reduce #(or %1 %2) false
    (map component-enable?
      (:components component))))

(defmethod component-enable? 'interaction
  [component]
  (and (not (:export? (:port component)))
    (reduce #(and %1 %2) true
      (map (fn [t]
             (port-enable? (:source-component t)
               (:source t)))
        (:port-bindings component)))))


(defn enabled-components
  "List all enabled components of a compound components"
  [component]
  {:pre [(= 'compound (:type component))]}
  (filter component-enable?
    (:components component)))




;; Should be multi-method. Digging into atomic/compound/interaction through
;; a port will meet different things.
(defmulti inner-components
  "Reveal the inner parts of a component through the 'export' port."
  (fn [component port] (:type component)))

(defmethod inner-components 'atomic
  [component port]
  {:pre [(= true (:export? port))]
   :post [(= 1 (count %))]}
  #_ ("Will generate a list of one transition.")
  (filter
    (fn [t]
      (and (attrs-equal? (get-current component)
             (:from t)
             :name )
        (attrs-equal? port (:port t) :name )))
    (:transitions component)))


(defmethod inner-components 'compound
  #_ ("Generate a tuple (component/interaction, port) wrapped in a list.")
  [component port]
  {:post [(= 1 (count %))]}
  (let [tl (filter
             (fn [t]
               (attrs-equal?
                 port
                 (:target t)
                 :name ))
             (:port-bindings component))]
    (map (fn [t]
           [(:source-component t) (:source t)])
      tl)))
(defmethod inner-components 'interaction
  #_ ("Generate a list of one tuple (component/interaction, port).")
  [component port]
  (map
    (fn [t]
      [(:source-component t) (:source t)])
    (:port-bindings component)))


;;Interfaces
(defmulti get-time
  "Get the current time of a component
   or compute the current time of an interaction.
   and return the value."
  (fn [component]
    (:type component)))

(defmulti set-time
  "Set the time of a component
   or pass an time value to an interaction,
   then the interaction will pass the value (plus its time-gap) on."
  (fn [component time-tag]
    (:type component)))

(defmulti get-time-via-port
  "Get the time of inner component who connects to the 'export' port."
  (fn [component port]
    (:type component)))
#_ (defmulti set-time-via-port
     "Set the time of inner component who connects to the 'export' port."
     (fn [component port time-tag]
       (:type component)))


;;Immplementations
(defn max-time
  [& time-tags]
  {:pre [(not-empty time-tags)
         "fn max-time: parameter 'time-tags' is empty."]}
  (reduce max 0 time-tags))

(defn min-time
  [& time-tags]
  {:pre [(not-empty time-tags)
         "fn min-time: parameter 'time-tags' is empty."]}
  (reduce min (first time-tags) time-tags))

(defmethod get-time 'atomic
  [component]
  (deref (:time-tag component)))

(defmethod get-time 'compound
  [component]
  (apply min-time
    (map get-time
      (:components component))))

(defmethod get-time 'interaction
  [component]
  (apply max-time
    (map (fn [t]
           (get-time-via-port
             (:source-component t)
             (:source t)))
      (:port-bindings component))))

(defmethod get-time-via-port 'atomic
  [component port]
  (get-time component))

(defmethod get-time-via-port 'compound
  [component port]
  (let [t
        (first (inner-components component port))]
    (apply get-time-via-port t)))

(defmethod get-time-via-port 'interaction
  [component port]
  (let [tl
        (inner-components component port)]
    (apply max-time
      (map (fn [t]
             (apply get-time-via-port t))
        tl))))

(defmethod set-time 'atomic
  [component time-tag]
  (compare-and-set! (:time-tag component)
    (get-time component)
    time-tag))

;; This method may be wrongly implemented.
#_ (defmethod set-time 'compound
     [component time-tag]
     (compare-and-set! (:time-tag component)
       (get-time component)
       time-tag))

#_ (defmethod set-time 'interaction
     [component time-tag]
     (let [current-time (+ time-tag
                          (:time-gap component))
           tl (:port-bindings component)]
       (reduce #(and %1 %2)
         true
         (map (fn [t]
                (set-time-via-port
                  (:source-component t)
                  (:source t)
                  current-time))
           tl))))

#_ (defmethod set-time-via-port 'atomic
     [component port time-tag]
     (let [t
           (first (inner-components component
                    port))]
       (set-time component
         (+ time-tag
           (:time-gap t)))))

#_ (defmethod set-time-via-port 'compound
     [component port time-tag]
     (let [t
           (first (inner-components component
                    port))]
       (apply set-time-via-port
         (into t [time-tag]))))

#_ (defmethod set-time-via-port 'interaction
     [component port time-tag]
     (let [tl (inner-components component
                port)
           current-time (+ time-tag
                          (:time-gap component))]
       (reduce #(and %1 %2)
         true
         (map (fn [t]
                (set-time-via-port
                  (:source-component t)
                  (:source t)
                  current-time))
           tl))))
