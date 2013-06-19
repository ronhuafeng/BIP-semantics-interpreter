(ns semantics-interpreter.protocols.Queryable)

(defprotocol Queryable
  (enable?
    [this] [this port] [this place port])
  (equal-name?
    [this that])
  (export?
    [this])
  (contain-port?
    [this port]))
