(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.

# Introduce
Scheduler is a selector which decides one from a sequence using a strategy like find and search functions. In Scheduler,
a scheduler is form by a map data structure which includes decided function, update function, all configures, requests or
sub scheduler.

## Request format:
There are 2 data structure to describe a request.
1. Number vector format ([1 2 0]): Each element represents a request and the value is request priority.
2. Map vector format ([{:req 1} {:req 2} {:req 0}]): Each element is a map who includes a :req field at least. And the :req value is request priority.

## Grant map fields:
Scheduler uses a map data structure described a grant.
* pri: Grant priority. 0 means invalid grant. The priority is the more with the pri larger
* lvl: Scheduler level for hierachy scheduler
* index: The position of grant in all requests
There may be others fields which adds by the user.

## Scheduler 

A scheduler is a map and should include follow fields:
* :run : A schedule run function which takes a cfg-map and one or more nodes parameters format and return a grant map. like:
```
(fn [cfg-map    ;scheduler configure map
     & nodes]   ;shcuduler requestion collection
  {:pri   ;Grant priority. 0 means none grant
   :index}) ;Grant position in requestion
  
```
* :update : A schedule update configures function after :fun function which takes a schedule map and a grant map parameters and returns a new schedule map. like:
```
(fn [sc           ;Old scheduler map
     gnt]         ;Grant map
  new-sc-map)
```

For simplly description, a custom scheduler should classify parameters with below types:
- RD: Used by 'run' function and defined by 'defsch' macro in sch-cfg-map.
- RI: Used by 'run' function and created in 'run' function inner. Need not defined.
- UD: Used by 'update' function and defined by 'defsch' macro in sch-cfg-map.
- RR: Used by 'run' function and defined by 'request' or 'request-grp' macro


Scheduler uses special scheduler DSL syntax descripted a scheduler.

Syntax: sch-dsl ::= [sch sch-cfg-map <middleware middleware-cfg-map ...> <sub-sch-dsl>]
Where:
* sch: A name of scheduler
* sch-cfg-map: A map for configure value of a scheduler. The keyword is dependent with scheduler and detail please refer to
               scheduler parameter definition document (RD and UD types).
* middleware: Middleware symbol for extend function
* sub-sch-dsl: Sub-scheduler define. It has the same syntax with sch-dsl


### defsch macro
defsch macro translates scheduler DSL into map structure which include follow fields:

* :subs : Sub requestions vector. There are three formats for subs:
** Number vector: A vector includes one or more requestions which represented by a priority number. likes: [1 2 0 3 4 1]
** Request map vector: A map vector who includes a :req request priority number value at least. likes: [{:req 1} {:req 3}]
** Sub schedule map format: A sub scheduler map vector.

* Others configure fields is maped into this scheduler map. 

# API
  There are below scheduler processing function provieds in these namespace:
  * sch: Create scheduler macro.
  * defsch: Create and define scheduler macro.
  * schedule: Run scheduler one time.
  * request: Modify scheduler request

# Scheduler
 This namespace includes below scheduler:
 * wrr: {:ptrs :get-ptr :update-ptr :weights :dec-func :max-req}

# Middleware
  * calendar: {:calendar-ptr :calendar-tbl}
  * backpressure: {:bp}

# Example

## Example-Simple WRR scheduler application
```
(defsch sc [wrr])   ;Define a WRR scheduler named sc
(request sc [0 1 1 1])   ;Add 4 sub request in sc scheduler
(def gnt (schedule sc))     ;Run scheduler one time and get the grant and updated status.
                     ;=> gnt:{:pri 1 :index 1 :lvl 0} 
```

## Example-Hierachy WRR scheduler application
```
(defsch sc [wrr [wrr] [wrr]])
(request sc [0] [0 1 1 1])
(request sc [1] [0 0 1 1])
(def gnt (schedule sc))
```"}
  scheduler.core)
  

(defmacro sch
  "Creates a scheduler map using scheduler DSL by scs."
  ([scs] `(sch ~scs 0))
  ([scs lvl]
   (let [funs (->> scs (filter symbol?))
         cfg (->> scs (filter map?) (into {}))
         subs (->> scs (filter vector?) (map #(list 'sch % (inc lvl))) vec)
         func (if (< 1 (count funs)) (cons '->> funs) (first funs))
         rst (assoc cfg :subs subs :lvl lvl)]
     `(  into ~rst ~func))))

(defmacro defsch
  "Define and create a scheduler map from scheduler DSL by scs named with name. Note: name is a atom structure."
  [name scs]
  `(def ~name (atom (sch ~scs))))

(defn- runsch
  "Runs scheduler sc and return a grant."
  [sc]
  (cond
    (number? sc) {:pri sc}
    (contains? sc :pri) sc
    (contains? sc :run) (apply (get sc :run) sc (get sc :subs))
    :else (throw (Exception. (str sc " is not a valid request format.")))))

(defn- upsch
  [sc gnt]
  (let [curr-node ((get sc :update) sc gnt)
        index (get gnt :index)
        sub-node (get-in sc [:subs index])
        sub? (get sub-node :run)
        sub-sc (when sub? (upsch sub-node (get gnt :sub)))]
    (if sub?
      (assoc-in curr-node [:subs index] sub-sc)
      curr-node)))

(defn schedule
  "Runs scheduler one time, update sc and return grant map."
  [sc]
  (let [gnt (runsch @sc)]
    (swap! sc upsch gnt)
    gnt))

(defn request
  "Modify the scheduler sc's request fields where hierachy is pos vector."
  ([sc pos req]
   (let [v (interpos :subs pos)
           v (cons :subs v)]
     (swap! sc assoc-in v req))))


(defn request-grp
  "Modify the scheduler sc's request fields where hierachy is pos vector."
  ([sc req]
   (swap! sc [:subs] req))
  ([sc pos req]
   (let [v (interpos :subs pos)
           v (cons :subs v)
           v (conj v :subs)]
     (swap! sc assoc-in v req))))

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
           weights (repeat (count nodes) 1)
           wgts weights
           get-ptr (fn [sch-cfg-map gnt] (get (get sch-cfg-map ptrs [0]) (get gnt :pri 0) 0))}
      :as sch-cfg-map} 
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
               gnt (runsch node)    ;运行当前结点，获取结果Grant
               ;wgt (if wgts-lst (first wgts-lst) nil)   ;获取当前调度权重
               ;wgt (if wgt wgt 1)      ;若没有wgts配置（标准RR）或者wgt配置为nil（总是有权重）或者wgts配置项不够，则设置权重为1
               wgt (first wgts-lst) ;获取当前调度权重
               weight (first weights-lst)
               wrr-reload-time (wrr-weight-reload-time wgt weight)    ;计算当前节点需要reload的时间。
               last-reload-time (get rst-gnt :wrr-reload-time)        ;获取上一个选中的节点的reload时间
               ;生成调度结果
               {:keys [pri] :as gnt} (into gnt {:sub gnt :lvl lvl :index index :wrr-reload-time wrr-reload-time})
               ptr (get-ptr sch-cfg-map gnt)     ;获取当前节点的指针
               next-index (rem (inc index) nodes-num)]
           (cond (and (pos? wgt) (>= pri max-req)) 
                  gnt   ;只要找到第一个有权重，并且最高优先级节点，则直接返回
                (<= pri 0)   ;无效请求，跳过
                  (recur rst-gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                (or (= nil last-reload-time) (< wrr-reload-time last-reload-time))  ;WRR有权重者覆盖无权重者
                  (recur gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                (and (== wrr-reload-time last-reload-time) (> pri (:pri rst-gnt)))  ;都有权重，优先级高者被选择
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


;=======================================================================================================
; Middleware
;=======================================================================================================

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
  * calendar-ptr: The TDM pointer for calendar configure table (calendar-tbl).
  * calendar-tbl: The schedule configure table which includes 3 fields:
  
  ### Return
    Return the node who is desided."
  [handler]
  {:run
    (fn [{:keys [calendar-ptr calendar-tbl lvl] :as cfg} & nodes]
      (let [{:keys [ptr work-conserver?] :or {ptr 0 work-conserver? true}} (get calendar-tbl calendar-ptr 0)
            gnt (->> ptr (get nodes) runsch)]
        (cond (pos? (:pri gnt)) (assoc gnt :calendar? true :lvl lvl)
              work-conserver? (assoc (apply (get handler :run) cfg nodes) :calendar? false)
              :else {:pri 0 :index 0 :lvl lvl :calendar? true})))
   :update
    (fn [{:keys [calendar-ptr calendar-tbl] :as sc} gnt]
      (let [next-ptr (-> calendar-ptr inc (rem (count calendar-tbl)))]
        (if (and (->> gnt :pri pos?) (->> gnt :calendar? not))
          (assoc ((get handler :update) sc gnt) :calendar-ptr next-ptr)
         (assoc sc :calendar-ptr next-ptr))))})

(defn backpressure
  "## Backpressure Middleware.
  
  ### Parameters:
  * bp: Backpressure flag. Assert backpressure when bp > 0."
  [handler]
  {:run
    (fn [{:keys [bp lvl] :as cfg} & nodes]
      (if (pos? bp)
        {:pri 0 :index 0 :lvl 0}
        (apply (get handler :run) cfg nodes)))
   :update
    (fn [sc gnt]
      ((get handler :update) sc gnt))})
  