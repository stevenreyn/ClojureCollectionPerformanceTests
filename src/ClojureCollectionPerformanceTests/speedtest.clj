(ns speedtest)


(import 'net.slreynolds.ds.IntMapSource)
(import 'net.slreynolds.ds.SomeValue)
(import 'net.slreynolds.ds.Result)
(import 'scala.Tuple2) 

; Warmup the jit compiler etc in the JVM
; before doing any measurements
(defn warmup[setup dowork N]
  (let [computation 7]
       (dotimes [_ N]
         (let [o (setup)
               res (dowork o)
               newComp (+ computation (. res getIntParam))]
           newComp))))
           
(defn get-memory-used[]
  (let [runt (Runtime/getRuntime)]
          (System/gc)
          (System/gc)
          (System/gc)
          (Thread/sleep 5000)
          (System/gc)
          (System/gc)
          (Thread/sleep 5000)
          (. runt totalMemory)))
                
                
; Measure space (bytes) consumed by result of
; of dowork
(defn measure-space[setup dowork]
  (let [computation 7
        o (setup)
        before (get-memory-used)
        res (dowork o)
        after (get-memory-used)
        otherComp (+ computation (. res getIntParam))]
    (println "otherComp" otherComp)
    (Math/max 0 (- after before))))

   
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
        (conj times (* (- after before) 1.E-9))
        (recur (dec ctr) (conj times (* (- after before) 1.E-9)) (+ computation (. res getIntParam)))))))      
       
; Run tests: warmup, measure-space, and then timeit
; Print results
(defn run [setup dowork title]
  (println "runing" title)
  (let [res (warmup setup dowork 20)]
    (println "res " res)
    (let [space1 (measure-space setup dowork)
          space2 (measure-space setup dowork)
          space3 (measure-space setup dowork)
          times (timeit setup dowork 5)]
      (println title "uses" space1 "," space2 "," space3 "bytes")
      (println "Times (seconds)")
      (println times)
      (println "thanks, all done now"))))
        
(defn create-some-value[#^Long i]
  (SomeValue. i))

; setup implementation for std clojure hashmap
(defn htsetup[]
  (let [initial-is (range 0 (- (IntMapSource/initialSize) 1))
        themap {}]
    (loop [newmap themap
           is initial-is]
      (if (empty? is)
        newmap
        (recur (assoc newmap (first is) (create-some-value (first is))) (rest is))))))


; dowork implementation for std clojure hashmap
(defn htdowork[themap]
  (let [valuesAsScalaArray (IntMapSource/valuesToInsert)
        n (IntMapSource/initialSize)]
    (loop [newmap themap
           i 0] 
      (if (>= i n)
        (Result. newmap 7654321)
        (recur (assoc newmap (. (aget valuesAsScalaArray i) _1) (. (aget valuesAsScalaArray i) _2)) (inc i))))))

; Run the test for std clojure hashmap
(defn htrun[]
  (run htsetup htdowork "Clojure hash map"))
