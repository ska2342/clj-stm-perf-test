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

(defn run-stm-example-from-website
  "Run the example from http://clojure.org/refs without any validation.
Could probably improve concurrency by using a random generator per thread."
  []
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

(defn run-rapid-fire
  "Use the same function on 4 Refs which swaps the values of two Refs (reading
them first) and increments and decrements the other two.  Processing is done on
some agents.  Validation occurs after all agents have finished."
  []
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
             (apply await agts)
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
    (average-time 15 rapid)))

(defn run-reader-vs-writer
  "Create writers and readers for 3 refs with a ratio of 2:1 and process them on
some agents. Readers validate current state of the refs. Writers just ref-set
new integers.

Could probably improve concurrency by using a random generator per thread."
  []
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
             (apply await agts)
             [@r1 @r2 @r3]))]
    (average-time 15 rvsw)))

(defn run-shared-int
  "This was inspired by the example in the Master's Thesis cited in the README
  of this project.  Validates result after agents finished."
  []
  (letfn [(shared-int
           []
           (let [r      (ref 0)
                 maxv   100000 ; must be divisable by n-agts for validation
                 agt-fn (fn [incs]
                          (dotimes [_ incs]
                            (dosync (alter r inc))))
                 n-agts 4
                 agts   (for [_ (range n-agts)]
                          (agent (int (/ maxv n-agts))))]
             (doseq [a agts]
               (send a agt-fn))
             (apply await agts)
             (if-not (= maxv (deref r))
               (throw (RuntimeException. "Shared int failed")))))]
    (average-time 10 shared-int)))


;; yes one would usually break this down into several functions.  It's just, I
;; want this file to contain only run-* functions which contains everything
;; necessary.  That's easier to paste to POCR (Plain Old Clojure REPL) which is
;; my main method of running these benchmarks
(defn run-stock-exchange
  "Stupid stock trade simulation with a market of symbols with a random number
of shares each.  Persons will be simulated by agents, each of which performs a
given number of trading transactions, buying a share from the market or giving
it back.  The coolest thing of this stock exchange is, that the people need no
money to trade.

Could probably improve concurrency by using a random generator per thread."
  []
  (letfn
      [(stock-exchange
        [n-pers n-trades]
        (let [n-shares 100              ; maximum number of shares per symbol
              n-syms   9                ; number of symbols in the market
              syms (vec (map #(format "%04d" %) (range n-syms)))
              ;; the market is just a ref on a map with symbols as keys and the
              ;; number of available shares as values
              market (ref
                      (into {} (map
                                (fn [sym] {sym (rand-int n-shares)})
                                syms)))
              ;; for validation: sum has to be constant
              mark-sum (reduce + (vals (deref market)))
              ;; these are the trading people, each with an unused :id and a
              ;; :portfolio of shares
              persons (ref (vec (map
                                 (fn [i] {:portfolio {} :id i})
                                 (range n-pers))))
              ;; agents to act as persons
              agts (vec (map #(agent %) (range n-pers)))

              ;; pick a symbol from the market, plus num available
              select-from-market #(let [sym (rand-nth syms)
                                        in-market (get @market sym)]
                                    [sym in-market])

              ;; pick a symbol from a person's :portfolio and the number of
              ;; those shares in the :portfolio
              select-from-person #(let [portfolio (:portfolio %)
                                        sym (rand-nth (keys portfolio))
                                        in-portfolio (get portfolio sym)]
                                    [sym in-portfolio])

              ;; buy from market to person
              buy (fn [pers-id]
                    (dosync
                     (let [pers (nth @persons pers-id)
                           [sym in-market] (select-from-market)]
                       ;;(println "Buy: " pers sym in-market)
                       (when (< 0 in-market)
                         (alter market #(update-in % [sym] dec))
                         (alter
                          persons
                          (fn [ps]
                            (if-not (get-in ps [pers-id :portfolio sym])
                              (assoc-in ps
                                        [pers-id :portfolio sym]
                                        1)
                              (update-in ps
                                         [pers-id :portfolio sym]
                                         inc)))))))
                    pers-id)
              ;; sell from person to market
              sell (fn [pers-id]
                     (dosync
                      (let [pers (nth @persons pers-id)
                            [sym in-portf] (select-from-person pers)]
                        ;;(println "Sell: " pers sym in-portf)
                        (when (and (not (nil? sym))
                                   (not (nil? in-portf))     
                                   (< 0 in-portf))
                          (alter market #(update-in % [sym] inc))
                          (alter persons
                                 #(update-in %
                                             [pers-id :portfolio sym]
                                             dec)))))
                     pers-id)] ; end let
          (doseq [a agts]
            (dotimes [i n-trades]
              (send a (if (= 2 (mod i 3)) sell buy))))
          (apply await agts)
          ))] ; end letfn
    (average-time 10 #(stock-exchange 100 500))))

(defn run-all []
  (let [start (System/nanoTime)]
    (println "stm example from website")
    (run-stm-example-from-website)
    (println "rapid fire")
    (run-rapid-fire)
    (println "reader vs writer")
    (run-reader-vs-writer)
    (println "shared int")
    (run-shared-int)
    (println "stock exchange")
    (run-stock-exchange)
    (println
     "All runs took" (/ (- (System/nanoTime) start) 1000000.0)
     "msecs")))
  
