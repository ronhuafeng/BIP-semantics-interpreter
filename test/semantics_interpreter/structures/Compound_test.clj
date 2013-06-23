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


(deftest all-in-one-test3-with-vars
  (let [P1 (create-port "P1" true [:x ])
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
             0
             {:x 1 :y 1})

        P2 (create-port "P2" true [:x ])
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
             0
             {:x 1 :y 2})

        EI (create-port "EI" true)
        I1 (create-interaction
             "I1"
             EI
             [{:component c1 :port P1}
              {:component c2 :port P2}]
             2
             (fn action!
               [direction]
               (cond
                 (= direction 'up)
                 {:x1 (:x (first (retrieve-port c1 P1)))
                  :x2 (:x (first (retrieve-port c2 P2)))}

                 (= direction 'down)
                 (fn [v]
                   ;; v is result of up-action
                   {P1 {:x (+ (:x1 v) (:x2 v))}
                    P2 {:x (+ (:x1 v) (:x2 v))}}))))

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

        (is (= 2 (get-variable c1 :x )))
        (is (= 2 (get-variable c2 :x )))

        (fire! c3)
        (fire! c3)

        (is (not (enable? c1)))
        (is (not (enable? c2)))
        (is (not (enable? I1)))
        (is (enable? I1 EI))
        (is (enable? c3 EC))))))

(deftest all-in-one-test4-with-vars-hierarchy
  (let [P1 (create-port "P1" true [:x ])
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
             0
             {:x 2 :y 1})

        P2 (create-port "P2" true [:x ])
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
             0
             {:x 2 :y 2})

        EI (create-port "EI" true [:x1 ])
        I1 (create-interaction
             "I1"
             EI
             [{:component c1 :port P1}
              {:component c2 :port P2}]
             2
             (fn action!
               [direction]
               (cond
                 (= direction 'up)
                 {:x1 (:x (first (retrieve-port c1 P1)))
                  :x2 (:x (first (retrieve-port c2 P2)))}

                 (= direction 'down)
                 (fn [v]
                   ;; v is result of up-action
                   {P1 {:x (+ (:x1 v) (:x1 v))}
                    P2 {:x (+ (:x1 v) (:x1 v))}}))))

        EC (create-port "EC" true [:x1 ])
        c3 (create-compound
             "c3"
             [c1 c2 I1]
             [{:target EC :source EI :source-component I1}])

        P4 (create-port "P4" true [:x ])
        state (create-place "state")
        tloop (create-transition "tloop" state state P4 1)
        c4 (create-atomic
             "c4"
             [P4]
             [state]
             state
             [tloop]
             0
             {:x 1})

        I2 (create-interaction
             "I2"
             nil
             [{:component c3 :port EC}
              {:component c4 :port P4}]
             2
             (fn action!
               [direction]
               (cond
                 (= direction 'up)
                 {:x1 (:x1 (first (retrieve-port c3 EC)))
                  :x (:x (first (retrieve-port c4 P4)))}

                 (= direction 'down)
                 (fn [v]
                   ;; v is result of up-action
                   {EC {:x1 (- (:x1 v) 1)}
                    P4 {:x (+ (:x1 v) 1)}}))))]
    (testing "all-in-one test 2 of Compount"
      (do
        (is (not (enable? c1)))
        (is (not (enable? c2)))
        (is (not (enable? I1)))
        (is (enable? I1 EI))
        (is (enable? c3 EC))
        (is (= (get-export c3 EC)
              {:source EI :source-component I1 :target EC}))
        (is (= [{:time 0 :x1 2}] (retrieve-port I1 EI)))
        (is (= [{:time 0 :x1 2}] (retrieve-port c3 EC))))

      (do
        (is (= ((:action! I2) 'up)
              {:x1 2 :x 1}))
        (is (= (((:action! I2) 'down) {:x1 2 :x 1})
              {EC {:x1 1} P4 {:x 3}}))
        (fire! I2)

        (is (= 2 (get-variable c1 :x )))
        (is (= 2 (get-variable c2 :x )))
        (is (= 3 (get-variable c4 :x )))))))
