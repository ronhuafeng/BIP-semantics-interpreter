(ns semantics-interpreter.structures.Interaction-test
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.protocols.Accessible)
  (:require [clojure.test :refer :all ]
   [semantics-interpreter.structures.Atomic :refer :all ]
   [semantics-interpreter.structures.Transition :refer :all ]
   [semantics-interpreter.structures.Port :refer :all ]
   [semantics-interpreter.structures.Place :refer :all ]
   [semantics-interpreter.structures.Interaction :refer :all ]))


(deftest all-in-one-test
  (let [start (create-place "start")
        end (create-place "end")

        I1 (create-port "I1" false)
        I2 (create-port "I2" false)
        E1 (create-port "E1" true)

        t1 (create-transition "t1" start end I1)
        t2 (create-transition "t2" start end E1)
        t3 (create-transition "t3" end start I2)

        C1 (create-atomic
             "C1"
             [I1 I2 E1]
             [start end]
             start
             [t1 t2 t3]
             0)
        ;;------------------------------------;;
        idle (create-place "idle")
        run (create-place "run")

        R1 (create-port "R1" true)
        R2 (create-port "R2" false)

        t4 (create-transition "t4" idle run R1)
        t5 (create-transition "t5" run idle R2)

        C2 (create-atomic
             "C2"
             [R1 R2]
             [idle run]
             idle
             [t4 t5]
             0)
        ;;------------------------------------;;
        PG (create-port "PG" true)
        G1 (create-interaction
             "G1"
             nil
             [{:component C1 :port E1}
              {:component C2 :port R1}]
             1)
        G2 (create-interaction
             "G2"
             PG
             [{:component C1 :port E1}
              {:component C2 :port R1}]
             1)]
    (testing "all-in-one testing of Interaction"
      (is (enable? C1 E1))
      (is (enable? C2 R1))
      (is (enable? G1))
      (is (enable? G2 PG))
      (is (not= [] (retrieve-port G2 PG)))
      (do
        (assign-port! C1 E1 {:time 1})
        (is (= 1 (get-time C1)))
        (is (enable? C1))
        (fire! C1)
        (is (enable? C1))
        (is (enable? C1 E1))
        (set-time C1 0))
      (do
        (assign-port! C2 R1 {:time 2})
        (is (enable? C2))
        (fire! C2)
        (is (enable? C2 R1))
        (is (= 2 (get-time C2)))
        (set-time C2 0))
      (do
        (fire! G1)
        (is (enable? C1))
        (is (enable? C2))
        (is (= 1 (get-time C1)))
        (is (= 1 (get-time C2))))
      (do
        (fire! C1)
        (fire! C2)
        (is (enable? G1))
        (is (enable? G2 PG)))

      (do
        (assign-port! G2 PG {:time 0})
        (is (enable? C1))
        (is (enable? C2))
        (is (= 1 (get-time C1)))
        (is (= 1 (get-time C2))))
      (do
        (fire! C1)
        (fire! C2)
        (is (enable? G1))
        (is (enable? G2 PG)))
      (is (not= [] (retrieve-port G2 PG))))))

(deftest all-in-one-test-with-vars
  (let [start (create-place "start")
        end (create-place "end")

        I1 (create-port "I1" false)
        I2 (create-port "I2" false)
        E1 (create-port "E1" true [:x ])

        t1 (create-transition "t1" start end I1)
        t2 (create-transition "t2" start end E1)
        t3 (create-transition "t3" end start I2)

        C1 (create-atomic
             "C1"
             [I1 I2 E1]
             [start end]
             start
             [t1 t2 t3]
             0
             {:x 1})
        ;;------------------------------------;;
        idle (create-place "idle")
        run (create-place "run")

        R1 (create-port "R1" true [:x ])
        R2 (create-port "R2" false)

        t4 (create-transition "t4" idle run R1)
        t5 (create-transition "t5" run idle R2)

        C2 (create-atomic
             "C2"
             [R1 R2]
             [idle run]
             idle
             [t4 t5]
             0
             {:x 2})
        ;;------------------------------------;;
        PG (create-port "PG" true [:x1 ])
        G1 (create-interaction
             "G1"
             nil
             [{:component C1 :port E1}
              {:component C2 :port R1}]
             1
             (fn action!
               [direction]
               (cond
                 (= direction 'up)
                 {:x1 (:x (first (retrieve-port C1 E1)))
                  :x2 (:x (first (retrieve-port C2 R1)))}

                 (= direction 'down)
                 (fn [v]
                   ;; v is result of up-action
                   {E1 {:x (+ (:x1 v) (:x2 v))}
                    R1 {:x (+ (:x1 v) (:x2 v))}}))))
        G2 (create-interaction
             "G2"
             PG
             [{:component C1 :port E1}
              {:component C2 :port R1}]
             1
             (fn action!
               [direction]
               (cond
                 (= direction 'up)
                 {:x1 (:x (first (retrieve-port C1 E1)))
                  :x2 (:x (first (retrieve-port C2 R1)))}

                 (= direction 'down)
                 (fn [v]
                   ;; v is result of up-action
                   {E1 {:x (+ (:x1 v) (:x2 v))}
                    R1 {:x (+ (:x1 v) (:x2 v))}}))))]
    (testing "all-in-one testing of Interaction with vars."
      (is (enable? C1 E1))
      (is (enable? C2 R1))
      (is (enable? G1))
      (is (enable? G2 PG))
      (is (= [{:time 0 :x 2}] (retrieve-port C2 R1)))

      (do
        (assign-port! C1 E1 {:time 1})
        (is (= 1 (get-time C1)))
        (is (enable? C1))
        (fire! C1)
        (is (enable? C1))
        (is (enable? C1 E1))
        (set-time C1 0))

      (do
        (assign-port! C2 R1 {:time 2})
        (is (enable? C2))
        (fire! C2)
        (is (enable? C2 R1))
        (is (= 2 (get-time C2)))
        (set-time C2 0))

      (do
        (set-time C1 0)
        (is (= 1 (get-variable C1 :x )))
        (fire! C1) ;; now in state end.
        (fire! C1) ;; to refresh port time, now in state start.
        (is (= [{:x 1 :time 0}] (retrieve-port C1 E1)))

        (is (= ((:action! G1) 'up)
              {:x1 1 :x2 2}))
        (is (= (((:action! G1) 'down)
                {:x1 1 :x2 2})
              {E1 {:x 3}
               R1 {:x 3}}))
        (is (= (get (((:action! G1) 'down)
                     {:x1 1 :x2 2})
                 E1)
              {:x 3}))
        (fire! G1)
        (is (= 3 (get-variable C1 :x )))
        (is (enable? C1))
        (is (enable? C2))
        (is (= 1 (get-time C1)))
        (is (= 1 (get-time C2))))


      (do
        (set-time C1 0)
        (set-time C2 0)
        (set-variable C1 :x 1)
        (set-variable C2 :x 2)
        (fire! C1)
        (fire! C2)
        (is (enable? G1))
        (is (enable? G2 PG))
        (is (= ((:action! G2) 'up)
              {:x1 1 :x2 2}))
        (is (= [{:x1 1 :time 0}] (retrieve-port G2 PG))))

      (do
        (assign-port! G2 PG {:time 0})
        (is (enable? C1))
        (is (enable? C2))
        (is (= 1 (get-time C1)))
        (is (= 1 (get-time C2))))
      (do
        (fire! C1)
        (fire! C2)
        (is (enable? G1))
        (is (enable? G2 PG)))
      (is (not= [] (retrieve-port G2 PG))))))
