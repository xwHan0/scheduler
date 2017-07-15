(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.


# Introduce


"}
  scheduler.middleware)
  

(defn calendar
  "# Introduce:
   Calendar middleware is an algorithm who changes request priroity in some period. Calendar sets a period
   request change configure table named calendar-tbl. It is look like:
   ------------------------------------------------
   | period-time | request-priority-change-vector |
   ------------------------------------------------
   | 1           | [0 1 1 -2 -2]                  |
   | 1           | [2 2 -1 -1 -1]                 |
   | 10          | [0 0 0 0 0]                    |
   ------------------------------------------------


  ### Parameters:
  * nodes: request node map. Must include :req field. The value ok :req represents request content.
             0 means None request and position means a valid request. The value of request represents 
             the request priority. e.g. 7 > 3
  * calendar-tbl: The schedule configure table which includes 3 fields:
  
  ### Return
    Return the node who is desided."
  [handler]
  {:run
    (fn [{:keys [calendar-tbl lvl] :as cfg} ts & nodes]
      (let [{:keys [pri-changes] :or {pri-changes (vec (repeat (count nodes) 0))}} (get calendar-tbl (rem ts (count calendar-tbl)) 0)
            nodes (map (fn [node change] 
                        (cond (number? node) (+ node change)
                              (contains? node :req) (update node :req + change)
                              :else (update node :pri + change))))
            gnt (->> ptr (get nodes) runsch)]
            gnt
        ;(cond (pos? (:pri gnt)) (assoc gnt :calendar? true :lvl lvl)
        ;      work-conserver? (assoc (apply (get handler :run) cfg nodes) :calendar? false)
        ;      :else {:pri 0 :index 0 :lvl lvl :calendar? true})
        ))
   :update
    (fn [{:keys [] :as sc} gnt]
      ((get handler :update) sc gnt))})

(defn backpressure
  "## Backpressure Middleware.
  
  ### Parameters:
  * bp: Backpressure flag. Assert backpressure when bp > 0."
  [handler]
  {:run
    (fn [{:keys [bp lvl] :as cfg} ts & nodes]
      (if (pos? bp)
        (apply (get handler :run) (assoc cfg :req 0) ts nodes)
        (apply (get handler :run) cfg ts nodes)))
   :update
    (fn [sc gnt]
      ((get handler :update) sc gnt))})


(defn shaper
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

  "
  [handler]
  {:run
    (fn [{:keys [shp-token shp-ts shp-pir-func] 
          :or {shp-token 100 shp-ts 0 shp-pir-func #(- %1 (get %2 :shp-ts 0))} 
          :as cfg} 
         ts & nodes]
      (let [new-token (+ shp-token (shp-pir-fun ts cfg))
            new-cfg (if (pos? new-token) cfg (assoc cfg :req 0))
            new-cfg (assoc new-cfg :shp-token new-token :shp-ts ts)]
        (apply (get handler :run) new-cfg ts nodes)))
   :update
    (fn [{:keys [shp-token shp-dec-func shp-cbs shp-dfs] 
          :or {shp-dec-func (constantly 1) shp-cbs 100 shp-dfs -100}
          :as sc} 
         gnt]
      (let [dec-token (shp-dec-func sc gnt)
            new-token (- shp-token dec-token)
            new-token (if (> new-token shp-cbs) shp-cbs new-token)
            new-token (if (< new-token shp-dfs) shp-dfs new-token)]
        ((get handler :update) (assoc sc :shp-token new-token) gnt)))})  
        
        
        (defn backpressure
  "## Backpressure Middleware.
  
  ### Parameters:
  * bp: Backpressure flag. Assert backpressure when bp > 0."
  [handler]
  {:run
    (fn [{:keys [bp lvl] :as cfg} ts & nodes]
      (if (pos? bp)
        (apply (get handler :run) (assoc cfg :req 0) ts nodes)
        (apply (get handler :run) cfg ts nodes)))
   :update
    (fn [sc gnt]
      ((get handler :update) sc gnt))})


        
        
        