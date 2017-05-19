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
Scheduler uses special scheduler DSL syntax descripted a scheduler.

Syntax: sch-dsl ::= [sch sch-cfg-map <middleware middleware-cfg-map ...> <sub-sch-dsl>]
Where:
* sch: A symbol for scheduler
* middleware: Middleware symbol for extend function
* sub-sch-dsl: Sub-scheduler define. It has the same syntax with sch-dsl

### defsch macro
defsch macro translates scheduler DSL into map structure which include follow fields:
* :run : A schedule run function which takes a cfg-map and one or more nodes parameters format and return a grant map. like:
```
(fn [cfg-map    ;scheduler configure map
     & nodes]   ;shcuduler requestion collection
  {:pri   ;Grant priority. 0 means none grant
   :index ;Grant position in requestion
  })
```
* :update : A schedule update configures function after :fun function which takes a schedule map and a grant map parameters and returns a new schedule map. like:
```
(fn [sc           ;Old scheduler map
     gnt]         ;Grant map
  new-sc-map)
```
* :subs : Sub requestions vector. There are three formats for subs:
** Number vector: A vector includes one or more requestions which represented by a priority number. likes: [1 2 0 3 4 1]
** Request map vector: A map vector who includes a :req request priority number value at least. likes: [{:req 1} {:req 3}]
** Sub schedule map format: A sub scheduler map vector.

* Others configure fields is maped into this scheduler map. 

# API
  There are below function provieds in these namespace:
  * sch: Create scheduler macro.
  * defsch: Create and define scheduler macro.
  * schedule: Run scheduler one time.
  * wrr: Weighted Round Robin scheduler
  * request: Modify scheduler request

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
  scheduler.core
  )

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
  ([{:keys [ptr max-req wgts lvl weights] 
      :or {ptr 0 max-req 1 lvl 0}} 
    & nodes]
    (let [nodes-num (count nodes)
          ptr (rem ptr nodes-num)
          new-nodes (->> nodes (split-at ptr) reverse (apply concat))
          new-wgts (->> wgts (split-at ptr) reverse (apply concat))
          new-weights (->> weights (split-at ptr) reverse (apply concat))
          ]
      (loop [rst-gnt {:pri 0 :index 0 :lvl lvl}
             nodes-lst new-nodes
             wgts-lst new-wgts
             weights-lst new-weights
             index ptr]
        (if nodes-lst
          (let [node (first nodes-lst)
                gnt (runsch node)
                wgt (if wgts-lst (first wgts-lst) nil)
                wgt (if wgt wgt 1)      ;若没有wgts配置（标准RR）或者wgt配置为nil（总是有权重）或者wgts配置项不够，则设置权重为1
                weight (first weights-lst)
                wrr-reload-time (wrr-weight-reload-time wgt weight)
                last-reload-time (get rst-gnt :wrr-reload-time)

                {:keys [pri] :as gnt} (into gnt {:sub gnt :lvl lvl :index index :wrr-reload-time wrr-reload-time})
                next-index (rem (inc index) nodes-num)]
            (cond (and (pos? wgt) (>= pri max-req)) 
                    gnt   ;只要找到第一个有权重，并且最高优先级节点，则直接返回
                  (<= pri 0)   ;无效请求，跳过
                    (recur rst-gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                 (or (= nil last-reload-time) (< wrr-reload-time last-reload-time))
                    (recur gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                  (and (== wrr-reload-time last-reload-time) (> pri (:pri rst-gnt)))
                     (recur gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
                  :else  ;评级，请求优先级低
                    (recur rst-gnt (next nodes-lst) (next wgts-lst) (next weights-lst) next-index)
              ))
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

  * Parameters:
    - nodes: request node map. Must include :req field. The value ok :req represents request content.
             0 means None request and position means a valid request. The value of request represents 
             the request priority. e.g. 7 > 3
    - ptr: Round robin pointer among all nodes (Number). ptr desides the position where scheduler selects starting.
           Default is 0.
    - max-req: Max value of request (Number). This is for optimization of algorithm. Default is 1 (means no priority).
    - wgts: Request nodes weight vector. Ignore this parameter means none weight.
    - weights: Weight value for each node
    
  * Return
    Return the node who is desided."
   {:run wrr-run :update wrr-update})


;=======================================================================================================
; Middleware
;=======================================================================================================

(defn calendar
  "## Calendar schedule algorithm Middleware.
  
  ### Parameters:
  * nodes: request node map. Must include :req field. The value ok :req represents request content.
             0 means None request and position means a valid request. The value of request represents 
             the request priority. e.g. 7 > 3
  * calendar-ptr: The TDM pointer for calendar configure table (calendar-tbl).
  * calendar-tbl: The schedule configure table which includes 3 fields:
  ** work-conserver? : Whether decides another nodes when calendar has a none device.
  
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
  