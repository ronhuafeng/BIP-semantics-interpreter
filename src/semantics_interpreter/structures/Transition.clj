(ns semantics-interpreter.structures.Transition
  (use semantics-interpreter.protocols.Queryable))

(defrecord Transition
  [type name source target port time guard? action!])

(defn create-transition
  ([name source target port]
   (->Transition 'Transition
     name
     source
     target
     port
     0
     (fn [c]
       true)
     (fn [c]
       ())))
  ([name source target port time]
   (->Transition 'Transition
     name
     source
     target
     port
     time
     (fn [c]
       true)
     (fn [c]
       ())))
  ([name source target port time guard? action!]
   (->Transition 'Transition
     name
     source
     target
     port
     time
     (fn [variables]
       true)
     action!)))

#_ (fn t-action
     [c]
     (do
       (set-variable
         c
         :x (+ 1
              (get-variable c :x )))))

(extend-type Transition
  Queryable

  (enable? [this place port]
    (and
      (equal-name? place (:source this))
      (equal-name? port (:port this))))
  )

