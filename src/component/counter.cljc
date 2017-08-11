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
  * shp-pir-func: Increasing token action. Default is #(- %1 (get %2 :shp-ts 0)). The syntax is: (ts sc)=>inc-token-value. Used for :run and defined by DSL
  * shp-dec-func: Decreasing token action. The syntax is: (sc gnt)=>dec-token-value. Default is (constantly 1). Used for :update and defined by DSL
  * shp-cbs: Max value of shp-token. Default is 100. Used for :update and defiend by DSL.
  * shp-dfs: Min value of shp-token. Default is 100. Used for :update and defiend by DSL. Note this value should be a minus number.



"}
  component.counter
  )
  
(defn- parse-counter [cnt]
  (cond (number? cnt) {:cnt cnt :ts 0 :rate 1}
        (map? cnt) (into {:cnt 0 :ts 0 :rate 1} cnt)
        :else (throw (Exception. (str "Invalid counter format |" cnt "|.")))))

(defn- parse-config [args]
  (if (map? (first args))  ;Configured value set
    (into {:min-val 0 :max-val 9999999999 :cir 1 :recycle? true} (first args))   ;
    (apply hash-map args)))

(defn dec-cnt [counter dec-value & args]
  "
  # Introduce
  Decrease a value of dec-value for counter and return a counter.
  
  # Parameters:
  * counter: Operated counter. These are two formats for counter.
    - number format: The value of counter.
    - map format: Includes :cnt field at least.
     
  "
  (let [{:keys [min-val max-val]} (apply parse-config args)
        {:keys [cnt ts rate]} (parse-counter counter)
        new-cnt (- cnt dec-value)
        new-cnt (cond (> new-cnt max-val)
                        (if recycle?
                          (+ min-val (- new-cnt max-cnt))
                          (min max-val new-cnt))
                      (< new-cnt min-val)
                        (if recycle?
                          (- max-val (- min-val new-cnt))
                          (max min-val new-cnt))
                      :else new-cnt)]
    (if (map? counter)
      (assoc counter :cnt new-cnt)
      new-cnt)))


(defn- threshold-stage [cnt thds]
  (let [thds (->> thds (map #(vector %2 (inc %1)) (range)))
        thd (->> thds (filter #(>= cnt (first %))) last second)]
    (if thd thd 0)))

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
 ([cnt thd]
    (let [{:keys [cnt ts rate]} (parse-counter cnt)]
      (if (zero? rate) 0
        (if (>= cnt thd) 1 0))))
 ([cnt thd multi-thd-ofst-seq]
    (let [{:keys [cnt ts rate]} (parse-counter cnt)]
      (if (zero? rate) 0
        (let [high-thds (->> multi-thd-ofst-seq
                             (reductions +)
                             (map #(+ % thd))
                             (cons thd))
              high-stage (threshold-stage cnt high-thds)]
          high-stage))))
 ([cnt thd current-stage low-thd]
   (let [{:keys [cnt ts rate]} (parse-counter cnt)]
     (cond (zero? rate) 0
           (and (zero? current-stage) (>= cnt thd)) 1
           (and (== 1 current-stage) (<= cnt low-thd)) 0
           :else current-stage)))
  ([cnt thd multi-thd-ofst-seq current-stage low-thd-ofst-seq]
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


