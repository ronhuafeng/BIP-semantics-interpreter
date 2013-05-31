3(ns semantics-interpreter.data-structure)

;; TODO: type and name field may be defined as meta information

(defrecord Place 
    [type, ^String name])

(defrecord Port 
    [type, ^String name, ^boolean export?])

(defrecord Transition 
    [type, ^String name, ^Place from, ^Place to, ^Port port])

(defrecord AtomicComponent
    [type, ^String name, ^Place current, transitions])

(defrecord PortBinding 
    [type, ^String name, source-component, ^Port source, ^Port target ])

(defrecord Interaction
    [type, ^String name, port-bingdings, ^Port port])

(defrecord CompoundComponent
    [type, ^String name, components, port-bindings])

(defn create-place
  [^String name]
  (->Place 'place name))

(defn create-port
  [^String name export?]
  (->Port 'port name export?))

(defn create-transition
  [^Place from ^Place to ^Port port]
  (->Transition 'transition "" from to port))

(defn create-transitions
  [& t-list]
  (apply list t-list))

(defn create-atom
  [^String name ^Place current transitions]
  (->AtomicComponent 'atomic name (atom current) transitions))

(defn create-port-binding
  [component ^Port source ^Port target]
  (->PortBinding 'port-binding "" component source target))
(defn create-port-bindings
  [& b-list]
  (apply list b-list))

(defn create-interaction
  [^String name port-bindings ^Port port]
  (->Interaction 'interaction name port-bindings port))

(defn create-components
  [& c-list]
  (apply list c-list))

(defn create-compound
  [^String name components port-bindings]
  (->CompoundComponent 'compound name components port-bindings))

(defmulti get-current
  "Get the current place which represents the atomic component's state."
  (fn [component] (:type component)))

(defmethod get-current 'atomic
  [component]
  (deref (:current component)))
