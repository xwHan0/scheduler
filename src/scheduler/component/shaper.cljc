(ns
  ^{:author "xwHan"
    :doc "
Common Scheduler models for ESL in communication fields.


 "# Introduce
  Shaper Middleware.
  Shaper is like a filter.

  The symbol of shaper is show below:

                                    |  (shp-pir-func ts sc)
                                    |
                            ------------------
                            \                /
              orignal req    \              /
                    -------->>\  shp-token /------------>>  Turn on when shp-token>0
                               \----------/
                                \++++++++/
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
  scheduler.component.shaper)
  

(defprotocol IShaper
  "Standard shaper operations."
  (update [this ts dec-func & inc-func])
  (asset-bp [this] [this ins-func])
  (desset-bp [this])
  (threshold [this thds]))
  
(defrecord shaper [pir bes dfs token sta ts]
  IShaper
  (update
    ([this ts dec-func & inc-func]
      (let [ts-diff (- ts (:ts this))
            tk-inc (cond (not= (:sta this) :NORM) 0
                         inc-func (inc-func this ts-diff)
                         :else (* ts-diff (:pir this)))
            tk (- (+ (:token this) tk-inc) (dec-func))
            tk (min (:bes this) tk)
            tk (max (:dfs this) tk)
            shp (update-in this [:token] tk)
            shp (update-in shp [:ts] ts)]
        shp)))
  (asset-bp
    ([this ts & inc-func] 
      (let [shp (.update this ts (constantly 0) inc-func)]
        (update-in shp [:sta] :BP))))
  (desset-bp [this] (update-in this [:sta] :NORM))

(defn create-shp
  ([] (shaper. 0 1 -1 0 :NORM 0))
  ([pir] (shaper. pir 1 -1 0 :NORM 0))
  ([pir bes] (shaper. pir bes -1 0 :NORM 0))
  ([pir bes dfs] (shaper. pir bes dfs 0 :NORM 0)))



