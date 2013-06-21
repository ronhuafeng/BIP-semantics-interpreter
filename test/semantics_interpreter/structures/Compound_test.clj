(ns semantics-interpreter.structures.Compound-test
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.protocols.Accessible)
  (:require [clojure.test :refer :all ]
   [semantics-interpreter.structures.Compound :refer :all ]
   [semantics-interpreter.structures.Atomic :refer :all ]
   [semantics-interpreter.structures.Interaction :refer :all ]
   [semantics-interpreter.structures.Transition :refer :all ]
   [semantics-interpreter.structures.Port :refer :all ]
   [semantics-interpreter.structures.Place :refer :all ]))

;; exports >> {:target :source :component}

#_ (defn create-compound
     [name subcomponents exports]
     (->Compound 'Compound name subcomponents exports))

;; scenario 1
(let [P1 (create-port "P1" true)
      Q1 (create-port "Q1" false)
      start1 (create-place "start1")
      end1 (create-place "end1")
      t1 (create-transition "t1" start1 end1 P1)
      t2 (create-transition "t2" end1 start1 Q1)
      c1 (create-atomic
           "c1"
           [P1 Q1]
           [start1 end1]
           start1
           [t1 t2]
           0)

      P2 (create-port "P2" true)
      Q2 (create-port "Q2" false)
      start2 (create-place "start2")
      end2 (create-place "end2")
      t3 (create-transition "t3" start2 end2 P2)
      t4 (create-transition "t4" end2 start2 Q2)
      c2 (create-atomic
           "c2"
           [P2 Q2]
           [start2 end2]
           start2
           [t3 t4]
           0)

      I1 (create-interaction
           "I1"
           nil
           [{:component c1 :port P1}
            {:component c2 :port P2}]
           0)

      c3 (create-compound
           "c3"
           [c1 c2 I1]
           [])]
  ())

;; scenario 2
(let [P1 (create-port "P1" true)
      Q1 (create-port "Q1" false)
      start1 (create-place "start1")
      end1 (create-place "end1")
      t1 (create-transition "t1" start1 end1 P1)
      t2 (create-transition "t2" end1 start1 Q1)
      c1 (create-atomic
           "c1"
           [P1 Q1]
           [start1 end1]
           start1
           [t1 t2]
           0)

      P2 (create-port "P2" true)
      Q2 (create-port "Q2" false)
      start2 (create-place "start2")
      end2 (create-place "end2")
      t3 (create-transition "t3" start2 end2 P2)
      t4 (create-transition "t4" end2 start2 Q2)
      c2 (create-atomic
           "c2"
           [P2 Q2]
           [start2 end2]
           start2
           [t3 t4]
           0)

      EI (create-port "EI" true)
      I1 (create-interaction
           "I1"
           EI
           [{:component c1 :port P1}
            {:component c2 :port P2}]
           0)

      EC (create-port "EC" true)
      c3 (create-compound
           "c3"
           [c1 c2 I1]
           [{:target EC :source EI :source-component I1}])]
  ())

(deftest all-in-one-test1
  (let [P1 (create-port "P1" true)
        Q1 (create-port "Q1" false)
        start1 (create-place "start1")
        end1 (create-place "end1")
        t1 (create-transition "t1" start1 end1 P1 0)
        t2 (create-transition "t2" end1 start1 Q1 1)
        c1 (create-atomic
             "c1"
             [P1 Q1]
             [start1 end1]
             start1
             [t1 t2]
             0)

        P2 (create-port "P2" true)
        Q2 (create-port "Q2" false)
        start2 (create-place "start2")
        end2 (create-place "end2")
        t3 (create-transition "t3" start2 end2 P2 1)
        t4 (create-transition "t4" end2 start2 Q2 2)
        c2 (create-atomic
             "c2"
             [P2 Q2]
             [start2 end2]
             start2
             [t3 t4]
             0)

        I1 (create-interaction
             "I1"
             nil
             [{:component c1 :port P1}
              {:component c2 :port P2}]
             1)

        c3 (create-compound
             "c3"
             [c1 c2 I1]
             [])]
    (testing "all-in-one test 1 of Compount"
      (is (not (enable? c1)))
      (is (not (enable? c2)))
      (is (enable? I1))
      (is (enable? c3))
      (do
        (fire! c3)
        (is (= 1 (get-time c1)))
        (is (= 2 (get-time c2)))
        (fire! c1)
        (fire! c2)
        (is (= 2 (get-time c1)))
        (is (= 4 (get-time c2)))))))


(deftest all-in-one-test2
  (let [P1 (create-port "P1" true)
        Q1 (create-port "Q1" false)
        start1 (create-place "start1")
        end1 (create-place "end1")
        t1 (create-transition "t1" start1 end1 P1)
        t2 (create-transition "t2" end1 start1 Q1)
        c1 (create-atomic
             "c1"
             [P1 Q1]
             [start1 end1]
             start1
             [t1 t2]
             0)

        P2 (create-port "P2" true)
        Q2 (create-port "Q2" false)
        start2 (create-place "start2")
        end2 (create-place "end2")
        t3 (create-transition "t3" start2 end2 P2 1)
        t4 (create-transition "t4" end2 start2 Q2)
        c2 (create-atomic
             "c2"
             [P2 Q2]
             [start2 end2]
             start2
             [t3 t4]
             0)

        EI (create-port "EI" true)
        I1 (create-interaction
             "I1"
             EI
             [{:component c1 :port P1}
              {:component c2 :port P2}]
             2)

        EC (create-port "EC" true)
        c3 (create-compound
             "c3"
             [c1 c2 I1]
             [{:target EC :source EI :source-component I1}])]
    (testing "all-in-one test 2 of Compount"
      (is (not (enable? c1)))
      (is (not (enable? c2)))
      (is (not (enable? I1)))
      (is (enable? I1 EI))
      (is (enable? c3 EC))
      (is (not= [] (retrieve-port c3 EC)))

      (do
        (assign-port! c3 EC {:time 2})
        (is (enable? c1))
        (is (enable? c2))
        (is (enable? c3))
        (is (not (enable? I1)))

        (is (= 4 (get-time c1)))
        (is (= 5 (get-time c2)))

        (fire! c3)
        (fire! c3)

        (is (not (enable? c1)))
        (is (not (enable? c2)))
        (is (not (enable? I1)))
        (is (enable? I1 EI))
        (is (enable? c3 EC))))))
