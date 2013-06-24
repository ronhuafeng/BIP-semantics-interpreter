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
      t1 (create-transition "t1" start end I1 0)
      t2 (create-transition "t2" start end E1 1)
      t3 (create-transition "t3" end start I2 0)
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
        t1 (create-transition "t1" start end I1 0)
        t2 (create-transition "t2" start end E1 1)
        t3 (create-transition "t3" end start I2 0)
        C1 (create-atomic
             "C1"
             [I1 I2 E1]
             [start end]
             start
             [t1 t2 t3]
             0)]
    (testing "all-in-one test of atomic component"
      (do
        (is (= 0 (get-time C1)))
        (set-time C1 2)
        (is (= 2 (get-time C1)))
        (set-time C1 0)
        (is (= 0 (get-time C1))))

      (is (= start (current-place C1)))
      (is (not= end (current-place C1)))
      (is (= (set [t1 t2]) (set (current-transitions C1))))
      (is (= (set [E1 I1]) (set (map :port (current-transitions C1)))))

      (is (not= [] (retrieve-port C1 E1)))
      (is (= [] (retrieve-port I2)))
      (is (not= [] (retrieve-port E1)))
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
        (is (not= [] (retrieve-port C1 E1)))
        (is (= [] (retrieve-port I2)))
        (is (not= [] (retrieve-port E1)))
        (is (= [] (retrieve-port C1 E2)))

        (do
          (assign-port! C1 E1 {:time 0})
          (is (= 1 (get-time C1)))

          (fire! C1)
          (assign-port! C1 E1 {:time 2})
          (is (= 3 (get-time C1))))))))

(deftest all-in-one-tests-with-vars
  (let [start (create-place "start")
        end (create-place "end")
        I1 (create-port "I1" false)
        I2 (create-port "I2" false)
        E1 (create-port "E1" true [:x ])
        E2 (create-port "E2" true)
        t1 (create-transition "t1" start end I1 0 true
                                   (fn t-action
                                     [c]
                                     (do
                                       (set-variable
                                         c
                                         :x (+ 1
                                              (get-variable c :x ))))))
        t2 (create-transition "t2" start end E1 1)
        t3 (create-transition "t3" end start I2 0)
        C1 (create-atomic
             "C1"
             [I1 I2 E1]
             [start end]
             start
             [t1 t2 t3]
             0
             {:x 1 :y 2})]
    (testing "all-in-one test with vars of atomic component"
      (do
        (is (= 0 (get-time C1)))
        (set-time C1 2)
        (is (= 2 (get-time C1)))
        (set-time C1 0)
        (is (= 0 (get-time C1))))

      (do
        (is (not= [] (retrieve-port C1 E1)))
        (is (= [] (retrieve-port I2)))
        (is (= [{:value {:x 1} :time 0}] (retrieve-port E1)))
        (is (= [] (retrieve-port C1 E2))))
      (do
        (is (= 1 (get-variable C1 :x )))
        (is (= 2 (get-variable C1 :y )))
        (is (= {:x 1} (project-value E1 (deref (:variables C1)))))
        (set-variable C1 :y 3)
        (is (= 3 (get-variable C1 :y )))
        )

      (is (= start (current-place C1)))
      (is (not= end (current-place C1)))
      (is (= (set [t1 t2]) (set (current-transitions C1))))
      (is (= (set [E1 I1]) (set (map :port (current-transitions C1)))))


      (is (= true (enable? C1)))
      (is (= true (enable? C1 E1)))
      (is (= true (contain-port? C1 E1)))
      (is (not= true (contain-port? C1 E2)))
      (do
        (fire! C1)
        (is (= 2 (get-variable C1 :x )))
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
        (is (not= [] (retrieve-port C1 E1)))
        (is (= [] (retrieve-port I2)))
        (is (not= [] (retrieve-port E1)))
        (is (= [] (retrieve-port C1 E2)))

        (do
          (assign-port! C1 E1 {:time 0})
          (is (= 1 (get-time C1)))

          (fire! C1)
          (assign-port! C1 E1 {:value {:x 3} :time 2})
          (is (= 3 (get-time C1)))
          (is (= 3 (get-variable C1 :x ))))))))
