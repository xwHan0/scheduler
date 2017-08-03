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
  scheduler.component.shaper
  )
  

(defn dec-cnt [counter dec-value & args]
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

(defn threshold [cnt thds]
  (let [thds (cond  (sequential? thds) thds 
                    (number? thds) [thds]
                    :else (throw (Exception. (str "Parameter: |" thds "| is not a valid thds format."))))
                    
        {:keys [cnt rate]
         :or {rate 1}}
            (cond (number? cnt) {:cnt cnt}
                  (map? cnt) cnt
                  :else (throw (Exception. (str "Invalid counter format |" cnt "|."))))
        
        thds (->> thds sort (map #(vector %2 (inc %1)) (range)))
        thd (->> thds (filter #(>= cnt (first %))) last second)]
    (cond (zero? rate) 0 
          thd thd 
          :else 0)))

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
  
