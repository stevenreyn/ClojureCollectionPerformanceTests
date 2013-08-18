(ns speedtest)

;;; Caution: does not work yet
;;; Caution: even if it does run, the timing is vastly over estimated

(import 'net.slreynolds.ds.IntMapSource)
(import 'net.slreynolds.ds.SomeValue)
(import 'scala.Tuple2) ; TODO probably shouldn't leave this

; Warmup the jit compiler etc in the JVM
; before doing any measurements
(defn warmup[setup dowork N]
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
                _ (System/gc)
                _ (System/gc)
                _ (System/gc)
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
          before (System/nanoTime)
          res (dowork o)
          after (System/nanoTime)]
      (if (zero? ctr)
        (conj times (- after before))
        (recur (dec ctr) (conj times (- after before)) (+ computation (. res getIntParam)))))))      
       
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
        
(defn create-some-value[#^Integer i]
  (SomeValue. i))

; setup implementation for std clojure hashmap
(defn htsetup[]
  (let [is (range 0 (- (IntMapSource/initialSize) 1))
        vs (map create-some-value is)
        themap (hash-map (interleave is vs))]
    themap))

; dowork implementation for std clojure hashmap
(defn htdowork[themap]
  ; Note doing a lot of extra work here inside the timed function!
  ; Making a list ahead of time is very important!!!!
  (let [initial-vs (list (IntMapSource/valuesToInsert))]
    (loop [newmap themap
           vs initial-vs] 
      (if (empty vs)
        newmap
        (recur (assoc newmap (first vs)) (rest vs))))))

; Run the test for std clojure hashmap
(defn htrun[]
  (run htsetup htdowork "Clojure hash map"))
