(ns
  ^{:author "xwHan"
    :doc "
Common Scheduler models for ESL in communication fields.


# Introduce


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



