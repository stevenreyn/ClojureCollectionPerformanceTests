(ns speedtest)

;;; Caution: does not work yet; times are wrong, space is wrong.
;;; Caution: the work in the timed function has lots of overhead

(import 'net.slreynolds.ds.IntMapSource)
(import 'net.slreynolds.ds.SomeValue)
(import 'net.slreynolds.ds.Result)
(import 'scala.Tuple2) ; TODO probably shouldn't leave this

; Warmup the jit compiler etc in the JVM
; before doing any measurements
(defn warmup[setup dowork N]
  (println "warming up" N)
  (let [computation 7]
       (dotimes [_ N]
         (let [o (setup)
               res (dowork o)
               newComp (+ computation (. res getIntParam))]
           newComp))))
           

; Measure space (bytes) consumed by result of
; of dowork
(defn measure-space[setup dowork]
        (println "measuring space")
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
  (println "timeing it")
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
    (print "res " res)
    (let [space (measure-space setup dowork)
          times (timeit setup dowork 5)]
      (println title "uses" space "bytes")
      (println "Times (seconds)")
      (println times)
      (println "thanks, all done now"))))
        
(defn create-some-value[#^Long i]
  (SomeValue. i))

; setup implementation for std clojure hashmap
(defn htsetup[]
  (println "htsetup")
  (let [initial-is (range 0 (- (IntMapSource/initialSize) 1))
        themap {}]
    (loop [newmap themap
           is initial-is]
      (if (empty is)
        newmap
        (recur (assoc newmap (first is) (create-some-value (first is))) (rest is))))))


; dowork implementation for std clojure hashmap
(defn htdowork[themap]
  (println "htdowork")
  ; Note doing a lot of extra work here inside the timed function!
  ; Making a list ahead of time is very important!!!!
  (let [initial-vs (list (IntMapSource/valuesToInsert))]
    (loop [newmap themap
           vs initial-vs] 
      (if (empty vs)
        (Result. newmap 7654321)
        (recur (assoc newmap (. (first vs) _1) (. (first vs) _2)) (rest vs))))))

; Run the test for std clojure hashmap
(defn htrun[]
  (run htsetup htdowork "Clojure hash map"))
