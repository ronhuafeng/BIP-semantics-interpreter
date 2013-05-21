;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(ns default)

(reduce (fn [ret x] (+ ret (inc x))) (+) [1 2 3])
(reduce (fn [ret x] (+ ret (inc x))) [1 2 3])
(reduce + (map inc [1 2 3]))
(reduce + [1 2 3])
(reduce + 2 [1])

(def b [1 2])

(def tr (fn [ret x] (+ ret (inc x))))
(tr 1 3)

(def ac ['m 'n])


(= ac [0 1])
(def ttt {:m 1})
(ttt :m)

(def aa [[1] [] [2] [3 4]])
(into (first  aa) (first aa))

(reduce into [] aa)

(find ["P"] "P")
(= 'm 'm)
(def a 'm)
a

(= a 'm)

(contains? {:name 'm} :name)

(and true false)
(= false false)

(if nil 1 3)

(def aaa {:type 'atomic})
(def bbb {:type 'atomic})

(= (aaa :type) (bbb :type))
(= (aaa :type) 'atomic)

(defn f [ret x]
  (+ ret x))

(f 1 2)
(+)
(*)
(and)
(reduce + (filter even? (map inc [1 1 1 2])))

(require '[clojure.core.reducers :as r])

(def red (comp (r/filter even?) (r/map inc)))
(reduce + (red [1 2 3]))

(into [] (red [1 2 3 4 5]))
(r/fold + [1 2 3 4])

(r/fold + (r/filter odd? (r/map inc [1 2 3 4])))
(r/fold + (r/map inc [1 2 3 4]))
(map inc [1 2 3 4])

(inc 1)
(+ 1 2 3)
(reduce + [1 2 3])
