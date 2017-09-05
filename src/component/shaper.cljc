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
  component.shaper
  (:require [component.counter :as counter]))
  

(defn- parse-config [args]
  (if (map? (first args))  ;Configured value set
    (into {:min-val 0 :max-val 9999999999 :cir 1 :pir 1} (first args))   ;
    (apply hash-map args)))


(defn update
"
Update shaper token.
"
  [{:keys [ctk ptk ts rate] :or {ctk 0 ptk 0 ts 0 rate 1} :as shaper} 
   extract nts & args]
  (let [{:keys [pir cir max-val min-val]} (apply parse-config args)
        ts-diff (- ts nts)
        nptk (+ (* pir ts-diff) extract)
        nctk (+ (* cir ts-diff) extract)]
    (if (zero? rate)
      (assoc shaper :ts nts)
      (assoc shaper :ts nts 
        :ctk (apply counter/dec-cnt ctk nctk args)
        :ptk (apply counter/dec-cnt ptk nptk args)))))
      
(defn shapern
  [{:keys [ctk ptk]}]
  (cond (>= ctk 0) 2
        (>= ptk 0) 1
        :else 0))      
      
(defn flowctrl
  ([rate] rate)
  ([counter rate nts & args]
    (assoc (update counter nts args) 
           :rate rate)))
