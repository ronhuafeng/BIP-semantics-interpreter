(ns semantics-interpreter.protocols.Accessible)

(defprotocol Accessible
  (retrieve-port
    [this]
    [this port])
  (assign-port!
    [this port token])
  (add-value!
    [this value])
  (clear!
    [this])
  (enable!
    [this])
  (current-place
    [atomic])
  (top-priority
    [this rules selections])
  (get-time
    [this]
    [this port])
  (set-time
    [this new-value]
    [this port new-value]))
