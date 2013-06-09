(ns semantics-interpreter.dataset-test)

(use 'semantics-interpreter.data-structure)
;; define some useful data instances
;; the creation of components can be simplified if the factory function
;; can use a map as its parametern
;;(with-redefs)
(def component-1
  (create-atom
    "component-1"
    (create-place "start")
    (create-transitions
      (create-transition (create-place "start")
        (create-place "end")
        (create-port "P" true)
        1)
      (create-transition (create-place "start")
        (create-place "end")
        (create-port "S" true)
        1)
      (create-transition (create-place "end")
        (create-place "start")
        (create-port "Q" true)
        2))
    0))
(def component-2
  (create-atom
    "component-2"
    (create-place "start")
    (create-transitions
      (create-transition (create-place "start")
        (create-place "end")
        (create-port "I" false)
        1)
      (create-transition (create-place "start")
        (create-place "end")
        (create-port "M" true)
        2)
      (create-transition (create-place "end")
        (create-place "start")
        (create-port "N" true)
        3))
    0))

(def interaction-1
  (create-interaction "interaction-1"
    (create-port-bindings
      (create-port-binding component-1
        (create-port "P" true)
        (create-port "P" false))
      (create-port-binding component-2
        (create-port "M" true)
        (create-port "M" false)))
    (create-port "T" true)
    0))
(def interaction-2
  (create-interaction "interaction-2"
    (create-port-bindings
      (create-port-binding component-1
        (create-port "S" true)
        (create-port "S" false)))
    (create-port "ES" true)
    0))

(def component-top
  (create-compound "component-top"
    (create-components component-1
      component-2
      interaction-1)
    (create-port-bindings
      (create-port-binding component-1
        (create-port "Q" true)
        (create-port "Q" true))
      (create-port-binding component-1
        (create-port "S" true)
        (create-port "S" true))
      (create-port-binding component-2
        (create-port "N" true)
        (create-port "N" true))
      (create-port-binding interaction-1
        (create-port "T" true)
        (create-port "T" true)))
    0))


