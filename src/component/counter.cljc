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
  

(defn dec-cnt [counter dec-value & args]
  "
  # Introduce
  Decrease a value of dec-value for counter and return a counter.
  
  # Parameters:
  * counter: Operated counter. These are two formats for counter.
    - number format: The value of counter.
    - map format: Includes :cnt field at least.
     
  "
  (let [{:keys [min-val max-val]   ;Parse configure values
         :as cfg}  ;Parse configure values
            (if (map? (first args))  ;Configured value set
              (first args)   ;
              (apply hash-map args))   ;Combinite rest parameters into a map data structure
        
        {:keys [cnt ts rate]
         :or {cnt 0 ts 0 rate 1}
         :as counter}
            (cond (number? counter) {:cnt cnt}
                  (map? counter) counter
                  :else (throw (Exception. (str "Invalid counter format |" counter "|."))))
        
        new-cnt (- cnt dec-value)
        new-cnt (if max-val (min max-val new-cnt) new-cnt)
        new-cnt (if min-val (max min-val new-cnt) new-cnt)]
    (if (map? counter)
      (assoc counter :cnt new-cnt)
      new-cnt)))

(defn- parse-counter [cnt]
  (cond (number? cnt) {:cnt cnt :ts 0 :rate 1}
        (map? cnt) (into {:cnt 0 :ts 0 :rate 1} cnt)
        :else (throw (Exception. (str "Invalid counter format |" cnt "|.")))))

(defn- threshold-stage [cnt thds]
  (let [thds (->> thds (map #(vector %2 (inc %1)) (range)))
        thd (->> thds (filter #(>= cnt (first %))) last second)]
    (if thd thd 0)))

(defn threshold
"
# Usage
## Parameters
* cnt: counter.
* thds: Threshold. There are two format:
  - number: Threshold value.
  - sequence: [max-threshold stage1-ofst stage2-ofst ...]
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

(defprotocol PCounter
  "Define stardand counter operators."
  (Decrease [this value & cfgs]
    "
    ## Parameters
    * args: Extends parameters list. Includes:
      - args[0]: Max value of counter.
      - args[1]: Min value of counter. 
    ")
  (Threshold [this thds]
    " - thds: One or more thresholds. For example thds is [50 80 100]:
        * If cnt is belong in (-inf, 50)  => 0
        * If cnt is belong in [50, 80) => 1
        * If cnt is belong in [80, 100) => 2
        * else => 3
    "))
    
(defn decrease [cnt dec-value & args]
  (let [cfg (if (map? (first args))
              (first args)
              (apply hash-map args))
        {:keys [min-val max-val]} cfg
        new-cnt (- cnt dec-value)
        new-cnt (if max-val (min max-val new-cnt) new-cnt)
        new-cnt (if min-val (max min-val new-cnt) new-cnt)]
    new-cnt))



(defrecord counter [cnt])

(extend counter
  PCounter
  { :Decrease 
      (fn [{:keys [cnt] :as this} value & cfgs]
        (let [new-cnt (apply decrease cnt value cfgs)]
          (counter. new-cnt)))

    :Threshold
      (fn [{:keys [cnt]} thds]
        (threshold cnt thds))})

(defn new-counter
  ([] (->counter 0))
  ([init] (->counter init)))
  
  
  
  
  
;
(defprotocol PShaper
  (Update [this nts cfg] [this nts cfg dec-tk])
  (Flowctrl [this fc]))

(defrecord shaper [tk rate ts])

(extend shaper
  PCounter
  {:Decrease
     (fn [{:keys [tk rate ts]} dec-val & cfgs]
       (shaper. (apply decrease tk dec-val cfgs) rate ts))
   :Threshold
     (fn [{:keys [tk rate]} thds]
       (if (zero? rate) 0 (threshold tk thds)))}
  PShaper
  {:Update
     (fn [{:keys [tk rate ts] :as this} nts {:keys [pir] :as cfg}]
       (if (zero? rate)
         (shaper. tk rate nts)
         (let [new-tk (- ts nts)
               new-tk (* pir rate new-tk)
               new-tk (decrease new-tk new-tk cfg)]
           (shaper. new-tk rate nts))))
   :Flowctrl
     (fn [{:keys [tk rate ts] :as this} fc]
   ;    (if (zero? fc)
   ;      (update-shaper this nts))
       (shaper. tk fc ts))})

(defn new-shaper
  ([] (->shaper 0 1 0)))
  
