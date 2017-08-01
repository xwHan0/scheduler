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
  

(defprotocol PCounter
  "Define stardand counter operators."
  (decrease [this value] [this value cfg]
    "
    ## Parameters
    * args: Extends parameters list. Includes:
      - args[0]: Max value of counter.
      - args[1]: Min value of counter. 
    ")
  (threshold [this thds]
    " - thds: One or more thresholds. For example thds is [50 80 100]:
        * If cnt is belong in (-inf, 50)  => 0
        * If cnt is belong in [50, 80) => 1
        * If cnt is belong in [80, 100) => 2
        * else => 3
    "))
    
(defn decrease [cnt dec-value & {:keys [min-val max-val]}]
  (let [new-cnt (- cnt dec-value)
        new-cnt (if max-val (min max-val new-cnt) new-cnt)
        new-cnt (if min-val (max min-val new-cnt) new-cnt)]
    new-cnt))

(defn threshold [cnt thds]
  (let [thds (cond  (sequential? thds) thds 
                    (number? thds) [thds]
                    :else (throw (Exception. (str "Parameter: |" thds "| is not a valid thds format."))))
        thds (->> thds sort (map #(vector %2 (inc %1)) (range)))
        thd (->> thds (filter #(>= cnt (first %))) last second)]
    (if thd thd 0)))

(defrecord counter [cnt])

(extend counter
  PCounter
  { :decrease 
      (fn [{:keys [cnt] :as this} value & cfg]
        (let [new-cnt (decrease cnt value cfg)]
          (counter. new-cnt)))

    :threshold
      (fn [{:keys [cnt]} thds]
        (threshold cnt thds))})

(defn new-counter
  ([] (->counter 0))
  ([init] (->counter init)))
  
  
;
(defprotocol PShaper
  (update [this nts cfg] [this nts cfg dec-tk])
  (flowctrl [this fc]))

(defrecord shaper [tk rate ts])

(extend shaper
  PCounter
  {:decrease
     (fn [{:keys [tk rate ts]} dec-val & {:keys [min-val max-val pir] :as cfg}]
       (shaper. (decrease tk dec-val cfg) rate ts))
   :threshold
     (fn [{:keys [tk rate]} thds]
       (if (zero? rate) 0 (threshold tk thds)))}
  PShaper
  {:update
     (fn [{:keys [tk rate ts] :as this} nts {:keys [pir] :as cfg}]
       (if (zero? rate)
         (shaper. tk rate nts)
         (let [new-tk (- ts nts)
               new-tk (* pir rate new-tk)
               new-tk (decrease new-tk new-tk cfg)]
           (shaper. new-tk rate nts))))
   :flowctrl
     (fn [{:keys [tk rate ts] :as this} fc]
   ;    (if (zero? fc)
   ;      (update-shaper this nts))
       (shaper. tk fc ts))})

(defn new-shaper
  ([] (->shaper. 0 1 0)))
  
; (extend shp
;   IShaper
;   {:update
;     (fn update 
;       ([this nts flowctrl] (update this nts 0 flowctrl))
;       ([{:keys [ts tk pir-rate pir cbs dfs] 
;          :as this} 
;         nts dec-tk flowctrl 
;         & inc-func]
;         (let [ts-diff (- nts @ts)
;             tk-inc (cond (zero? @pir-rate) 0
;                          inc-func (inc-func this ts-diff)
;                          :else (* ts-diff pir @pir-rate))
;             tk (- (+ @tk tk-inc) dec-tk)
;             tk (min @cbs tk)
;             tk (max @dfs tk)
;             shp (swap! this assoc :tk tk)
;             shp (swap! this assoc :ts ts)
;             shp (swap! this assoc :pir-rate flowctrl)
;             ]
;         shp)))
;    })


; ;pir cbs dfs tk ts pir-rate]
; (defn shaper
;   ([& {:keys [pir cbs dfs] 
;        :or {pir 0 cbs 9999999 dfs 0}}] 
;     (shp. pir cbs dfs (atom 0) (atom 0) (atom 1)))
;   ([pir cbs] (shp. pir cbs 0 atom 0) (atom 0) (atom 1)))
;   ([pir cbs dfs] (shp. pir cbs dfs atom 0) (atom 0) (atom 1))))



