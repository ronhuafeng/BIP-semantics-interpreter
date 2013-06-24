(ns semantics-interpreter.structures.Token
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible))


(defn create-token
  ([value time]
   {:time time :value value}))
