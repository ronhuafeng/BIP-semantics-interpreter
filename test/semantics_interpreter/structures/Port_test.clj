(ns semantics-interpreter.structures.Port-test
  (use semantics-interpreter.protocols.Fireable)
  (use semantics-interpreter.protocols.Queryable)
  (use semantics-interpreter.protocols.Accessible)
  (:require [clojure.test :refer :all ]
   [semantics-interpreter.structures.Port :refer :all ]))

(deftest export?-test
  (testing "export? of Port"
    (is (= true (export? (create-port "P" true))))
    (is (= false (export? (create-port "Q" false))))))

(deftest add-token!-test
  (testing "add-token! of Port"
    (is (let [p (create-port "P" false)]
          (do
            (add-token! p {:x 1})
            (= [{:x 1}]
              (retrieve-port p)))))
    (is (let [p (create-port "P" false)]
          (do
            (add-token! p {:x 1})
            (= [{:x 1}]
              (retrieve-port p)))))
    (is (let [p (create-port "P" false)]
          (do
            (add-token! p {:ePort '()})
            (= [{:ePort '()}]
              (retrieve-port p)))))
    (is (let [p1 (create-port "P" false)
              p2 (create-port "Q" false)]
          (doseq [p [p1 p2]]
            (add-token! p 1))
          (= [1] (retrieve-port p1))))))

(deftest retrieve-port-test
  (testing "retrieve-port-test of Port"
    (is (let [p (create-port "P" false)]
          (do
            (add-token! p {:x 1})
            (= [{:x 1}]
              (retrieve-port p)))))))

(deftest clear!-test
  (testing "clear! of Port"
    (is (let [p (create-port "P" false)]
          (do
            (add-token! p {:x 1})
            (clear! p)
            (= []
              (retrieve-port p)))))))

(deftest equal-name?-test
  (testing "equal-name? of Port"
    (is (= true (equal-name? (create-port "P" false)
                  (create-port "P" false))))
    (is (= false (equal-name? (create-port "P" false)
                   (create-port "Q" false))))))

(deftest enable?-test
  (testing "enable? of Port"
    (is (= true (let [p (create-port "P" false)]
                  (do
                    (add-token! p {:x 1})
                    (enable? p)))))
    (is (= false (enable?
                   (create-port "P" false))))))
