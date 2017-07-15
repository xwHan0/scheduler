(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.


# Introduce


"}
  scheduler.sch)
  



;=============================================================================================================
; WRR scheduler
;=============================================================================================================

(defn- wrr-run
"Principle:
    1) Re-sequence request nodes using max-req priority pointer;
    2) Iterate each request node 
"
  ([{:keys [ptrs get-ptr max-req wgts lvl weights] 
      :or {ptrs [0] max-req 1 lvl 0
           weights (vec (repeat (count nodes) 1))
           wgts weights
           get-ptr (fn [sch-cfg-map gnt] (get (get sch-cfg-map ptrs [0]) (get gnt :pri 0) 0))}
      :as sch-cfg-map} 
    ts
    & nodes]
   (let [nodes-num (count nodes)    ;获取请求节点个数
         max-ptr (rem (get-ptr sch-cfg-map {:pri max-req}) nodes-num)   ;获取最高优先级指针
         new-nodes (->> nodes (split-at max-ptr) reverse (apply concat))  ;按最高优先级指针位置移位节点
         new-wgts (->> wgts (split-at max-ptr) reverse (apply concat))  ;
         new-weights (->> weights (split-at max-ptr) reverse (apply concat))
         ]
          
     (loop [rst-gnt {:pri 0 :index 0 :lvl lvl}    ;默认Grant
            nodes-lst new-nodes     ;循环每个节点
            wgts-lst new-wgts
            weights-lst new-weights
            index max-ptr       ;记录当前循环位置
            ]
       (if nodes-lst
         (let [node (first nodes-lst)     ;获取当前请求节点
              ;  gnt (runsch node)    ;运行当前结点，获取结果Grant
               ;wgt (if wgts-lst (first wgts-lst) nil)   ;获取当前调度权重
               ;wgt (if wgt wgt 1)      ;若没有wgts配置（标准RR）或者wgt配置为nil（总是有权重）或者wgts配置项不够，则设置权重为1
               wgt (first wgts-lst) ;获取当前调度权重
               weight (first weights-lst)
               wrr-reload-time (wrr-weight-reload-time wgt weight)    ;计算当前节点需要reload的时间。
               last-reload-time (get rst-gnt :wrr-reload-time)        ;获取上一个选中的节点的reload时间
               ;生成调度结果
               {:keys [req] :as gnt} (into (runsch ts node) {:sub gnt :index index :wrr-reload-time wrr-reload-time})
              ;  {:keys [pri] :as gnt} (->  gnt 
              ;                             (into {:sub gnt :lvl lvl :index index :wrr-reload-time wrr-reload-time})
              ;                             (into (->>  node
              ;                                         (remove #(-> val func?))
              ;                                         (dissoc :subs))))
               ptr (get-ptr sch-cfg-map gnt)     ;获取当前节点的指针
               next-index (rem (inc index) nodes-num)]
           (cond (and (pos? wgt) (>= req max-req)) 
                  gnt   ;只要找到第一个有权重，并且最高优先级节点，则直接返回
                (<= req 0)   ;无效请求，跳过
                  (recur rst-gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                (or (= nil last-reload-time) (< wrr-reload-time last-reload-time))  ;WRR有权重者覆盖无权重者
                  (recur gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                (and (== wrr-reload-time last-reload-time) (> req (:req rst-gnt)))  ;都有权重，优先级高者被选择
                  (recur gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                (let [lst-gnt-distance (- (:index rst-gnt) ptr)
                      lst-gnt-distance (if (< lst-gnt-distance 0) (+ lst-gnt-distance nodes-num) lst-gnt-distance)
                      new-gnt-distance (- index ptr)
                      new-gnt-distance (if (neg? new-gnt-distance) (+ new-gnt-distance nodes-num) new-gnt-distance)]
                  (< new-gnt-distance lst-gnt-distance))  ;优先级相同，距离指针近者优先被选择
                  (recur gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                :else  ;评级，请求优先级低
                  (recur rst-gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)))
              
         rst-gnt)))))

(defn- wrr-update
  [{:keys [ptr wgts dec-func weights]
    :or {ptr 0 dec-func (constantly 1)} 
    :as sc} 
   {:keys [index pri wrr-reload-time] 
    :as gnt}]
  (if (pos? pri)
    (if wgts
      (let [dec-wgt (dec-func gnt)
            wgts-new (update wgts index - dec-wgt)
            wgts-new (if (zero? wrr-reload-time) wgts-new
                     ;  (update wgts index - dec-wgt)
                       (vec (map #(+ %1 (* %2 wrr-reload-time)) wgts-new weights)))]
        (assoc sc :pri (inc index) :wgts wgts-new))
      (assoc sc :pri (inc index)))
    sc))

(def wrr
    "Round Robin schedule algorithm with weight and priority.

  * Parameters (RR/RD/UD/RI is a type of parameter. Detail please refer to scheduler.core namespace comment):
    - nodes<RR>: request node map. Must include :req field. The value :req represents request content.
             0 means None request and position means a valid request. The value of request represents 
             the request priority. e.g. 7 > 3.
             This is 'run' function parameter, and assign value in 'request' macro. 
    - ptrs<RD>: Round robin pointer collection which represents the selection starting position in prior among all nodes.
           'wrr-fun' function will create it formatted vector indexed request priority when ingore it.
           This is 'run' function parameter, and assign value in 'defsch' macro's sch-cfg-map.
    - get-ptr: A function represents how get the pointer from ptrs field. The form is: (req-node, pos) => ptr
          Default is get-ptr (fn [sch-cfg-map gnt] (get (get sch-cfg-map ptrs [0]) (get gnt :pri 0) 0))
          This is 'run' function parameter, and assign value in 'defsch' macro's sch-cfg-map.
    - update-ptr: A function represents how update the pointer to ptrs field. The form is: (req-node, gnt) => new-ptr
          Default is (fn [req-node, gnt] (inc (:index gnt)))
          This is 'update' function parameter, and assign value in 'defsch' macro's sch-cfg-map.
    - max-req<RD>: Max value of request (Number). This is for optimization of algorithm. Default is 1 (means no priority).
               This is 'run' function parameter, and assign value in 'defsch' macro's sch-cfg-map.
    - wgts<RI>: Request nodes weight vector (Number). 
                This is 'run' function parameter, and do not need initialization. It will be create and maintaned with 'run' function.
                Default is Fair Round Robin strategy.
    - weights<RD>: Weight vector (Number) for each node corresponding for wgts. Ignore this parameter means none weight.
    - dec-func<UD>: A function to represent how decrease WRR weight token. The format is (dec-fun gnt) => dec-token.
                    Gnt's format please refer to Grant Map fileds section in scheduler.core namespace comment.
                    Default is a function of '(constantly 1)'.
                    This is 'update' function parameter, and assign value in 'defsch' macro's sch-cfg-map.
    
  * Return
    Return the node who is desided.
    
  ## Example
  ```
    (defsch wrr-sc [wrr {:max-req 4 :weight [100 200 150 450]}])
    (request wrr-sc [1 3 2 4])
    (def gnt (schedule wrr-sc))   'gnt is result of schedule
  ```
  
  ## Symbolize
  The wrr scheduler is really a standand WRR scheduler with priority. It can be described using standand WRR as follow:

                                                  
                                                 |
                                            ------------
                                            |   SP     |
                                            ------------
                                            /    |    \\
                                          priN priN-1  pri0
                                          /      |      \\ 
                                      -------  -------  -------
                                      | WRR |  | WRR |  | WRR |
                                      -------  -------  -------  
                                      | | | |  | | | |  | | | |

  Here, a WRR schduler with max-req 3 can be represented by:
  
                                  |
                              ----------
                             /          \
                            /            \
                           |     WRR3     |
                    ptr--->|   <weight>   |<--- dec-func
                            \            /
                             \          /
                              ----------  
                              /    |    \
                             /     |     \ 
  "
   {:run wrr-run :update wrr-update})

