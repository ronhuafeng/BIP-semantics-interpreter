;; author: bef0rewind
;; date: 2013-05-17
;; last-modified: 2013-05-18

(ns semantics-interpreter.core
  (:use semantics-interpreter.dataset)
  (:use semantics-interpreter.state-queries)
  (:use semantics-interpreter.data-structure)
  (:use semantics-interpreter.fire))

(defn -main
  [& args]
  (println "hello\n")
  (println "hello\n")
  (pr-str component-1)
  (println "\n\n\n")
  (println (:name (get-current component-1)))
  (println (:name (get-current component-2)))
  (fire-component component-top)
  (println (:name (get-current component-1)))
  (println (:name (get-current component-2))))

(defn one
  []
  1)



