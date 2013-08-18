(ns net.slreynolds.ds.speedtest)

;;; Caution: does not work yet

; Warmup the jit compiler etc in the JVM
; before doing any measurements
(defn warmup[setup dowork]
  (let [computation 7]
       (dotimes [_ N]
         (let [o (setup)
               res (dowork o)
               newComp (+ computation (. res getIntParam))]
           newComp))))
           

; Measure space (bytes) consumed by result of
; of dowork
(defn measure-space[setup dowork]
      
        (let [computation 7
              o (setup)]
          (System/gc)
          (System/gc)
          (System/gc)
          (let [runt (Runtime/getRuntime)
                before (. runt totalMemory)
                res (dowork o)
                (System/gc)
                (System/gc)
                (System/gc)
                after (. runt totalMemory)
                otherComp (+ computation (. res getIntParam))]
            (Math/max 0 (- after before)))))

   
; Do setup and then dowork N times. Record the times taken in dowork.
; Return list of times
(defn timeit[setup dowork N]
  (loop [ctr (- N 1)
         times []
         computation 7]
    (let [o (setup)
          before (System/nanotime)
          res (dowork o)
          after (System/nanotime)
          (conj times (- after before))]
      (if (zero? ctr)
        times
        (recur ((dec ctr) times (+ computation (. res getIntParam))))))))
      
       
; Run tests: warmup, measure-space, and then timeit
; Print results
(defn run [setup dowork title]
  (let [res (warmup setup dowork 20)]
    (print "res " res)
    (let [space (measure-space setup dowork)
          times (timeit setup dowork 5)]
      (print title " uses " space " bytes ")
      (print "Times (seconds)")
      (print times))))
        
; setup implmentation for std clojure hashmap
(defn htsetup[]
  (let [is (range 0 (- IntMapSource/initialSize 1))
        vs (map #(SomeValue. %) is)
        themap (hash-map (interleave is vs))]
    themap))

; dowork implementation for std clojure hashmap
(defn htdowork[themap]
  ; Note doing a lot of extra work here!
  ; Making a list ahead of time is very important!!!!
  (let [list-of-vs (list IntMapSource/valuesToInsert)]
    (loop [newmap themap
           vs list-of-vs] 
      (if (empty vs)
        newmap
        (recur (assoc newmap (first vs) (rest vs)))))

; Run the test for std clojure hashmap
(defn htrun[]
  (run htsetup htdowork "Clojure hash map"))
