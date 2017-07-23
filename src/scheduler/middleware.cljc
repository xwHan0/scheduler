(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.


# Introduce


"}
  scheduler.middleware)
  
(defn counter []
  "Closure implementation of counter.
  
# Introduce
It is usefull as a pred function who statistics the times of something.

# Usage
It takes none parameters and returns a threshold compare function whose syntax is:
  [thd] => compare-result
  [thd inc-func & params] => compare-result
where:
  - thd: Threshold for compare.
  - inc-func: A function who take params as parameters and return a increasement quantity for counter. Ignore means a one increasement.

# Example

```
(def times (counter))
(if (times 6 + 2) "OK")  ;=> nil
(if (times 6 + 2) "OK")  ;=> nil
(if (times 6 + 2) "OK")  ;=> "OK"
```
"
  (let [cnt (atom 0)]
    (fn 
      ([thd]
        (swap! cnt inc)
        (let [rst (>= @cnt thd)]
          (if rst (reset! cnt 0))
          rst))
      ([thd inc-func & params]
        (swap! cnt + (apply inc-func params))
        (let [rst (>= @cnt thd)]
          (if rst (reset! cnt 0))
          rst)))))


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

 
        
        
(defn continous
  "## Backpressure Middleware.
  
  ### Parameters:
  * bp: Backpressure flag. Assert backpressure when bp > 0."
  [name trigle-pred finish-pred]
  (let [id (keyword (str "ctn_" name))]
    (fn [handler]
      {:run (fn [cfg ts & nodes])
       :update (fn [sc gnt]
                 (if (and (= :NORM (-> sc id :sta)) trigle)))}))
  )


        
        
        