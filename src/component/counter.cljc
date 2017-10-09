(ns
  ^{:author "xwHan"
    :doc "
Common Scheduler models for ESL in communication fields.

# Introduce
  Shaper Middleware.
  Shaper is like a filter.

  The symbol of shaper is show below:

                                    |  (shp-pir-func ts sc)
                                    |
                            ------------------
                            \\                /
              orignal req    \\              /
                    -------->>\\  shp-token /------------>>  Turn on when shp-token>0
                               \\----------/
                                \\++++++++/
                                 --------
                                    |
                                    |  (shp-dec-func sc gnt)
  
  # Parameters:
  * shp-token: Token of shaper. Default is 100. Used for :run, defined by DSL
  * shp-ts: Ts value of last update token. Default is 0. Used for :run and :update, defined by DSL
  * shp-pir-func: Increasing token acption. Default is #(- %1 (get %2 :shp-ts 0)). The syntax is: (ts sc)=>inc-token-value. Used for :run and defined by DSL
  * shp-dec-func: Decreasing token action. The syntax is: (sc gnt)=>dec-token-value. Default is (constantly 1). Used for :update and defined by DSL
  * shp-cbs: Max value of shp-token. Default is 100. Used for :update and defiend by DSL.
  * shp-dfs: Min value of shp-token. Default is 100. Used for :update and defiend by DSL. Note this value should be a minus number.



"}
  component.counter
  )
  

(defn dec-cnt [cnt dec-value & args]
  "
  # Introduce
  Decrease a value of dec-value for counter and return a counter.
  
  # Parameters:
  * counter: Operated counter. These are two formats for counter.
    - number format: The value of counter.
    - map format: Includes :cnt field at least.
     
  "
  (let [{:keys [min-val max-val recycle?]} (apply parse-config args)
        new-cnt (- cnt dec-value)
        new-cnt (cond (> new-cnt max-val)
                        (if recycle?
                          (+ min-val (- new-cnt max-cnt))
                          (min max-val new-cnt))
                      (< new-cnt min-val)
                        (if recycle?
                          (- max-val (- min-val new-cnt))
                          (max min-val new-cnt))
                      :else new-cnt)
    new-cnt))
    
(defn inc-cnt [counter value & args] (apply dec-cnt counter (- value) args))
(defn clear [counter & args] 0)

(defn large?
  [counter cmp & args]
  (let [{:keys [recycle? min-val max-val]} (apply parse-config args)
        {:keys [cnt]} (parse-counter counter)]
    (if recycle?
      (let [mid (- max-val min-val)
            lcnt (> cnt mid)
            nlcnt (not lcnt)
            lcmp (> cmp mid)
            nlcmp (not lcmp)
            ncnt (if lcnt (- cnt mid) (- cnt min-val))
            ncmp (if lcmp (- cmp mid) (- cmp min-val))]
        (cond (and lcnt lcmp) (>= ncnt ncmp)
              (and nlcnt nlcmp) (>= ncnt ncmp)
              (and lcnt nlcmp) (< ncnt ncmp)
              :else (< ncnt ncmp)))
      (>= cnt cmp))))

(defn- threshold-stage [cnt thds]
  (let [thds (->> thds (map #(vector %2 (inc %1)) (range)))
        thd (->> thds (filter #(>= cnt (first %))) last second)]
    (if thd thd 0)))
    
(defn- threshold-judge 
  ([cnt thd {:keys [min-thd-ofst max-rate] :or {min-thd-ofst 0 max-rate 100}}]
    (let [min-thd (- thd min-thd-ofst)]
      (cond (< cnt min-thd) false
            (>= cnt thd) true
            :else (let [x (- cnt min-thd)
                        y (* x (/ max-rate min-thd-ofst))
                        r (random 100)]
                    (>= r y)))))
  ([cnt thd status {:keys [low-thd-ofst] :or {low-thd-ofst 0}}]
    (if status
      (< cnt (- thd low-thd-ofst))
      (>= cnt thd))))

(defn threshold
"
# Usage
## Parameters
* cnt: counter.
* thd<number>: Base threshold.
* high-thd-ofst-seq: Multi thresholds offset from thd. 
  The format is: [stage1-ofst stage2-ofst ... stageN-ofst]
* current-stge: Current stage.
* low-thd-ofst-seq: Low threshold offsets from high thresholds.
  The format is: [stage0-ofst stage1-ofst stage2-ofst ... stageN-ofst]
## Return
  
"
 ([cnt]
    (let [{:keys [cnt ts rate]} (parse-counter cnt)]
      (if (zero? rate) 0
        (if (>= cnt 0) 1 0))))
 ([cnt thd]
    (let [{:keys [cnt ts rate]} (parse-counter cnt)]
      (if (zero? rate) 0
        (if (>= cnt thd) 1 0))))
 ([cnt thd & cfgs]
   (let [status? (number? (first cfgs))
         status (if status? (first cfgs) 0)
         cfgs (if status? (next cfgs) cfgs)
         rsts (if status?
                (map #(threshold-judge thd (> status %2) %1) cfgs (range))
                (map #(threshold-judge thd %) cfgs))]
     (loop [rst rsts idx (range)]
       (if rst
         (let [r (first rst)]
           (if (zero? r)
             (first idx)
             (recur (next rst) (next idx))))
         (count rsts)))))
 #_([cnt thd multi-thd-ofst-seq]
    (let [{:keys [cnt ts rate]} (parse-counter cnt)]
      (if (zero? rate) 0
        (let [high-thds (->> multi-thd-ofst-seq
                             (reductions +)
                             (map #(+ % thd))
                             (cons thd))
              high-stage (threshold-stage cnt high-thds)]
          high-stage))))
 #_([cnt thd current-stage low-thd]
   (let [{:keys [cnt ts rate]} (parse-counter cnt)]
     (cond (zero? rate) 0
           (and (zero? current-stage) (>= cnt thd)) 1
           (and (== 1 current-stage) (<= cnt low-thd)) 0
           :else current-stage)))
  #_([cnt thd multi-thd-ofst-seq current-stage low-thd-ofst-seq]
    (let [{:keys [cnt ts rate]} (parse-counter cnt)]
      (if (zero? rate) 0
        (let [high-thds (->> multi-thd-ofst-seq
                             (reductions +)
                             (map #(+ % thd))
                             (cons thd))
              low-thds (map #(- %1 %2) high-thds low-thd-ofst-seq)
              high-stage (threshold-stage cnt high-thds)]
          (cond (> high-stage current-stage) high-stage
                (< high-stage current-stage) (threshold-stage cnt low-thds)
                :else current-stage))))))

