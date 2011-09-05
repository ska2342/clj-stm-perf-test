(ns clj-stm-perf-test.core)

(defn average-time [n f]
  (println "Average time after " n "repetions:"
           (double
            (/
             (reduce
              +
              (map        
               (fn [x]
                 (let [start (System/nanoTime)]
                   (time (f))
                   (- (System/nanoTime) start)))
               (range n)))
             (* n 1000000)))))

(defn run-stm-example-from-website []
  (letfn [(run
           [nvecs nitems nthreads niters]
           (let [vec-refs (vec (map (comp ref vec)
                                    (partition nitems (range (* nvecs nitems)))))
                 swap #(let [v1 (rand-int nvecs)
                             v2 (rand-int nvecs)
                             i1 (rand-int nitems)
                             i2 (rand-int nitems)]
                         (dosync
                          (let [temp (nth @(vec-refs v1) i1)]
                            (alter (vec-refs v1) assoc i1 (nth @(vec-refs v2) i2))
                            (alter (vec-refs v2) assoc i2 temp))))
                 ]
             (dorun (apply pcalls (repeat nthreads #(dotimes [_ niters] (swap)))))
             (count (distinct (apply concat (map deref vec-refs))))))]
    (average-time 15 #(run 100 10 10 10000))))

(defn run-rapid-fire []
  (letfn [(impl
           []
           (let [size 50000
                 r1 (ref 0)
                 r2 (ref size)
                 r3 (ref [])
                 r4 (ref [:coin])
                 agts (take 10 (repeatedly #(agent 0)))
                 af (fn [a]
                      (dosync
                       (let [v3 (deref r3)
                             v4 (deref r4)]
                         (alter r1 inc)
                         (alter r2 dec)
                         (ref-set r3 v4)
                         (ref-set r4 v3))))]
             (dorun
              (map #(send % af)
                   (take size (cycle agts))))
             (doseq [a agts]
               (await a))
             [@r1 @r2 @r3 @r4 size]))
          (rapid
           []
           (let [[v1 v2 v3 v4 sz] (impl)]
             (if (or
                  (not= v1 sz)
                  (not= v2 0)
                  (not= v3
                        (if (= 1 (mod sz 2)) [:coin] []))
                  (not= v4
                        (if (= 0 (mod sz 2)) [:coin] [])))
               (throw (RuntimeException.
                       (str "Assertion failed "
                            [v1 v2 v3 v4 sz]))))))]
    (average-time 20 rapid)))    

(defn run-reader-vs-writer []
  (letfn [(rvsw
           []
           (let [n  50000
                 r1 (ref 100)
                 r2 (ref 200)
                 r3 (ref 300)
                 readr (fn [agt]
                         (dosync
                          (let [v1 @r1 v2 @r2 v3 @r3]
                            (if (not= 600
                                      (+ v1 v2 v3))
                              (throw
                               (RuntimeException. "Reader error"))))))

                 writr (fn [agt]
                         (let [x1 (rand-int 300)
                               x2 (- x1 (rand-int (int (/ x1 2))))
                               x3 (- 600 (+ x1 x2))]
                           (dosync
                            (ref-set r1 x1)
                            (ref-set r2 x2)
                            (ref-set r3 x3))))
                 agts (take 5 (repeatedly #(agent 0)))
                 ]
             (dorun
              (map #(send %2 (if (= 0 (mod %1 3)) readr writr))
                   (take n (range))
                   (take n (cycle agts))))
             (doseq [a agts]
               (await a))
             [@r1 @r2 @r3]))]
      (average-time 30 rvsw)))
    


(comment
  :reader-vs-writer
  (/ 381.39986096 298.06458444)
  :rapid-fire
  (/ 6227.3828961 5835.9593978)
  :website
  (/ 4878.0352188 3970.7327914))
        
                 
        
(defn run-all []
  (let [start (System/nanoTime)]
    (println "stm example from website")
    (run-stm-example-from-website)
    (println "rapid fire")
    (run-rapid-fire)
    (println "reader vs writer")
    (run-reader-vs-writer)

    (println
     "All runs took" (/ (- (System/nanoTime) start) 1000000.0)
     "msecs")))
  

  ;; (println "collision counter")
  ;; (run-collision-counter)
  ;; (println "history stresstest")
  ;; (run-history-stresstest))
