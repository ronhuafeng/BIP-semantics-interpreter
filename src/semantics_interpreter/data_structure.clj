(ns semantics-interpreter.data-structure)

(defrecord Place
  [type, ^String name])
(defrecord Port
  [type, ^String name, ^boolean export?])
(defrecord Transition
  [type, ^String name, ^Place from, ^Place to, ^Port port, time-gap])
(defrecord AtomicComponent
  [type, ^String name, ^Place current, transitions, time-tag])
(defrecord PortBinding
  [type, ^String name, source-component, ^Port source, ^Port target])
(defrecord Interaction
  [type, ^String name, port-bindings, ^Port port, time-gap])
(defrecord CompoundComponent
  [type, ^String name, components, port-bindings, time-tag])

(defn create-place
  [^String name]
  (->Place 'place name))

(defn create-port
  [^String name export?]
  (->Port 'port name export?))

(defn create-transition
  [^Place from ^Place to ^Port port time-gap]
  (->Transition 'transition "" from to port time-gap))

(defn create-transitions
  [& t-list]
  (apply list t-list))

(defn create-atom
  [^String name ^Place current transitions time-tag]
  (->AtomicComponent 'atomic name (atom current) transitions (atom time-tag)))

(defn create-port-binding
  [component ^Port source ^Port target]
  (->PortBinding 'port-binding "" component source target))

(defn create-port-bindings
  [& b-list]
  (apply list b-list))

(defn create-interaction
  [^String name port-bindings ^Port port time-gap]
  (->Interaction 'interaction name port-bindings port time-gap))

(defn create-components
  [& c-list]
  (apply list c-list))

(defn create-compound
  [^String name components port-bindings time-tag]
  (->CompoundComponent 'compound name components port-bindings (atom time-tag)))



