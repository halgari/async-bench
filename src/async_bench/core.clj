(ns async-bench.core
  (:require [clojure.core.async.impl.ioc-macros :as ioc]
            [criterium.core :refer [quick-bench]])
  (:gen-class))
;; This project is a benchmark of the inner workings of the go macro
;; The end goal is to exacerbate the slow-down of the ioc code and
;; then benchmark it against against more "normal" code. Most of the
;; time, the performance impact of ioc transformation won't even be
;; noticed, this project exists to keep tabs on where we lie
;; performance wise. 


(defmacro as-ioc
  [& body]
  `(let [state# (~(ioc/state-machine body 0 &env {}))]
     (ioc/run-state-machine state#)
     (ioc/aget-object state# ioc/VALUE-IDX)))

(defn ioc-count-to [max]
  (as-ioc (loop [x 0]
            (when (< x max)
              (recur (inc x))))))

(defn std-count-to [max]
  (loop [x 0]
            (when (< x max)
              (recur (inc x)))))


(defmacro clojure-bench [& exprs]
  #_`(time (dotimes [x# 10000]
             ~@exprs))
  `(quick-bench ~@exprs))


(defn bench-count-to []
  (println "Testing loop performance")
  (println "Standard:")
  (clojure-bench (std-count-to 10000))
  (println "IOC:")
  (clojure-bench (ioc-count-to 10000)))


;; yield code
(defn yield [state blk val]
  (ioc/aset-all! state
                 ioc/STATE-IDX blk
                 ioc/VALUE-IDX val)
  (cons val (lazy-seq (ioc/run-state-machine state))))

(defn lazy-seq-return [state val]
  (ioc/aset-all! state
                 ioc/STATE-IDX :finished
                 ioc/USER-START-IDX nil)
  nil)

(defmacro lazy-seq-gen
  [& body]
  `(let [state# (~(ioc/state-machine body 1 &env {'yield `yield
                                                  :Return `lazy-seq-return}))]
     (ioc/aset-all! state# ioc/BINDINGS-IDX (clojure.lang.Var/getThreadBindingFrame))
     (ioc/run-state-machine state#)))

(defn ioc-range [max]
  (lazy-seq-gen
   (loop [x 0]
     (when (< x max)
       (recur (inc (yield x)))))))


(defn std-range
  ([max]
     (std-range 0 max))
  ([x max]
     (when (< x max)
       (cons x (lazy-seq
                (std-range (inc x) max))))))

(definterface IIterator
  (current [])
  (moveNext []))

(defn yield-iterator [state blk val]
  (ioc/aset-all! state
                 ioc/STATE-IDX blk
                 ioc/VALUE-IDX val)
  nil)

(defn return-iterator [state val]
  ::done)

(defn make-iterator [state]
  (reify IIterator
    (current [this]
      (ioc/aget-object state ioc/VALUE-IDX))
    (moveNext [this]
      (if (nil? (ioc/run-state-machine state))
        true
        false))))

(defmacro iterator-gen
  [& body]
  `(let [state# (~(ioc/state-machine body 1 &env {'yield `yield-iterator
                                                  :Return `return-iterator}))]
     (ioc/aset-all! state# ioc/BINDINGS-IDX (clojure.lang.Var/getThreadBindingFrame))
     (make-iterator state#)))

(defn iterator-range [max]
  (iterator-gen
   (loop [x 0]
     (when (< x max)
       (recur (inc (yield x)))))))

(defn run-iterator-test [^IIterator i]
  (while (.moveNext i)
    (.current i)))

(defn bench-range-to []
  (println "Testing lazy-seq performance")
  (println "Standard:")
  (clojure-bench (assert (= (range 10000) (std-range 10000))))
  (println "IOC:")
  (clojure-bench (assert (= (range 10000) (ioc-range 10000))))
  (println "IOC Iterator:")
  (clojure-bench (run-iterator-test (iterator-range 10000))))

(defn -main []
  (println "Press ENTER to begin....")
  (read-line)
  
  (bench-count-to)
  (bench-range-to)

  (println "Done")
  (Thread/sleep 1000))
