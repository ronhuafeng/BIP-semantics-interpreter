(ns semantics-interpreter.protocols.Queryable)

(defprotocol Queryable
  (enable?
    [this] [this port])
  (equal? 
    [this that])
  (export?
    [this]))
