(ns semantics-interpreter.structures.Transition-test
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.protocols.Accessible)
  (:require [clojure.test :refer :all ]
   [semantics-interpreter.structures.Transition :refer :all ]
   [semantics-interpreter.structures.Place :refer :all ]
   [semantics-interpreter.structures.Port :refer :all ]))

;;TODO: find other value-binding scheme.
(deftest enable?-test
  (testing "enable? of Transition"
    (let [current (create-place "start")
          other (create-place "end")
          p0 (create-port "P" false)
          t1 (create-transition "t1" current current p0)
          p1 (create-port "P" false)
          p2 (create-port "Q" false)]
      (is (= true (enable? t1 current p1)))
      (is (= false (enable? t1 current p2)))
      (is (= false (enable? t1 other p1))))))
