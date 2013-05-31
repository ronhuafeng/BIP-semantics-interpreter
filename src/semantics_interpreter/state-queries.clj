(ns semantics-interpreter.state-queries)

(use 'semantics-interpreter.data-structure)
(use 'semantics-interpreter.dataset)

;; possible-ports component-enable? in? export-enable?
;; interaction-enable? 
;; bip-next-snapshot

(defn- attrs-equal?
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
                         (and (attrs-equal? (get-current component) (:from t) :name)
                              (attrs-equal? port (:port t) :name)))
                       (:transitions component)))))
(defmethod port-enable? 'compound
  [component port]
  (not (empty? (filter #(if (attrs-equal? port (:target %) :name)
                          (port-enable? (:source-component %) port)
                          false) 
                       (:port-bindings component)))))
(defmethod port-enable? 'interaction
  [component port]
  (if (attrs-equal? port (:port component) :name)
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
  (map :target
       (filter #(and (:export? (:target %))
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
  (reduce #(and %1 %2) true
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
