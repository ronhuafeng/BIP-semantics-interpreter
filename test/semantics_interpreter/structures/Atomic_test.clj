(ns semantics-interpreter.structures.Atomic-test
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.protocols.Accessible)
  (:require [clojure.test :refer :all ]
   [semantics-interpreter.structures.Atomic :refer :all ]
   [semantics-interpreter.structures.Transition :refer :all ]
   [semantics-interpreter.structures.Port :refer :all ]
   [semantics-interpreter.structures.Place :refer :all ]))

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
           0)]
  ())

(deftest all-in-one-tests
  (let [start (create-place "start")
        end (create-place "end")
        I1 (create-port "I1" false)
        I2 (create-port "I2" false)
        E1 (create-port "E1" true)
        E2 (create-port "E2" true)
        t1 (create-transition "t1" start end I1)
        t2 (create-transition "t2" start end E1)
        t3 (create-transition "t3" end start I2)
        C1 (create-atomic
             "C1"
             [I1 I2 E1]
             [start end]
             start
             [t1 t2 t3]
             0)]
    (testing "all-in-one test of atomic component"
      (is (= start (current-place C1)))
      (is (not= end (current-place C1)))
      (is (= (set [t1 t2]) (set (current-transitions C1))))
      (is (= (set [E1 I1]) (set (map :port (current-transitions C1)))))
      #_ (is (do
               (doseq [p [I1 E1]]
                 (add-value! p {:ePort '()}))
               (= [{:ePort '()}] (retrieve-port E1))))
      (is (= [{:ePort '()}] (retrieve-port C1 E1)))
      (is (= [] (retrieve-port I2)))
      (is (= [{:ePort '()}] (retrieve-port E1)))
      (is (= [] (retrieve-port C1 E2)))
      (is (= true (enable? C1)))
      (is (= true (enable? C1 E1)))
      (is (= true (contain-port? C1 E1)))
      (is (not= true (contain-port? C1 E2)))
      (do
        (fire! C1)
        (is (= end (current-place C1)))
        (is (= true (enable? C1)))
        (is (not= true (enable? C1 E1)))
        (is (= [] (retrieve-port C1 E1)))
        (is (= [] (retrieve-port I1)))


        (fire! C1)
        (is (= true (enable? C1)))
        (is (= true (enable? C1 E1)))
        (is (= true (contain-port? C1 E1)))
        (is (not= true (contain-port? C1 E2)))
        (is (= start (current-place C1)))
        (is (not= end (current-place C1)))
        (is (= [{:ePort '()}] (retrieve-port C1 E1)))
        (is (= [] (retrieve-port I2)))
        (is (= [{:ePort '()}] (retrieve-port E1)))
        (is (= [] (retrieve-port C1 E2)))))))
