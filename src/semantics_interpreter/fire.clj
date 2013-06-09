(ns semantics-interpreter.fire
  (:use semantics-interpreter.data-structure)
  (:use semantics-interpreter.state-queries)
  (:use semantics-interpreter.dataset))

(defn select-a-enabled-component
  "Select a component from a list of enabled components,
   the one with earliest time has the highest priority."
  [enabled-components]
  {:pre [(pos? (count enabled-components))
         "Empty list of enabled components is not allowed!"]}
  (let [tmin (apply min-time
               (map get-time
                 enabled-components))]
    (rand-nth (filter #(= tmin (get-time %))
                enabled-components))))


(defmulti fire-inner-component
  "Fire a component via its 'export'port.
   Return type: boolean"
  (fn [component port time-tag]
    (:type component)))

(defmethod fire-inner-component 'atomic
  [component port time-tag]
  (let [t (first (inner-components component port))]
    (and (set-current component
           (:to t))
      (set-time component
        (:time-gap t)))))

(defmethod fire-inner-component 'compound
  #_ ("Fire the component connected to the port.")
  [component port time-tag]
  (let [t (first
            (inner-components component
              port))]
    (apply fire-inner-component
      (into t [time-tag]))))

(defmethod fire-inner-component 'interaction
  #_ ("Fire every component connected to this interaction
      via their corresponding port.")
  [component port time-tag]
  (let [tl
        (inner-components component port)]
    (reduce #(and %1 %2)
      true
      (map #(apply fire-inner-component
              (into %
                [(+ time-tag
                   (:time-gap component))]))
        tl))))

(defmulti fire-component
  "Fire a component and set it to a new state.
   Return type: boolean"
  (fn [component]
    (:type component)))

(defmethod fire-component 'atomic
  [component]
  #_ ("select a transition to fire.")
  (let [tl (filter
             (fn [t]
               (and (attrs-equal? (get-current component)
                      (:from t)
                      :name )
                 (= false (:export? (:port t)))))
             (:transitions component))]
    (if (not-empty tl)
      #_ ("if new state set successfully, the function return true.")
      (let [t (rand-nth tl)]
        (and (set-current component
               (:to t))
          (set-time component
            (+ (get-time component)
              (:time-gap t)))))
      false)))
(defmethod fire-component 'compound
  [component]
  (let [sub-component
        (select-a-enabled-component
          (filter component-enable? (:components component)))]
    (fire-component sub-component)))

(defmethod fire-component 'interaction
  [component]
  (let [tl (:port-bindings component)
        time-tag (get-time component)]
    (reduce
      #(and %1 %2)
      true
      (map (fn [t]
             (fire-inner-component
               (:source-component t)
               (:source t)
               time-tag))
        tl))))
