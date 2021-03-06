(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.






* :update : A schedule update configures function after :fun function which takes a schedule map and a grant map parameters and returns a new schedule map. like:
```
(fn [sc           ;Old scheduler map
     gnt]         ;Grant map
  new-sc-map)
```


## Schedule Grant
':run' function translates schedule request map into grant map. In this process, 'runsch' macro will remove :subs field and all function fields from req map and ':run' function will append below fields:


Beside these, scheduler.core bypass all others fields to grant map. Note that ':run' function may append other field for some extend feature.

# Scheduler.core
Scheduler.core uses below componments to form a schedule tree.

## Scheduler 
A scheduler is a map represented a schedule process action and should include :run and :update fields (Refer to Schedule Request section).
Scheduler.core provides some standard scheduler. The user can custom and build new scheduler by self.

## Middleware
A extend wrap of scheduler is used to expand features of basic scheduler. A middleware is a function which takes a scheduler as sole parameter and return a map who has the same format and fileds (:run and :update) with a scheduler.

## params-map
Configured variables and process temporary variables for scheduler or middleware. 



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
```

"}
  scheduler.core)
  

(defmacro sch
  "Create and return a schedule request tree from schedule DSL 'scs'. It is simular with fn macro."
  ([scs] `(sch ~scs 0))
  ([scs lvl]
   (let [funs (->> scs (filter (or symbol? list?)))
         cfg (->> scs (filter map?) (into {}))
         subs (->> scs (filter vector?) (map #(list 'sch % (inc lvl))) vec)
         func (if (< 1 (count funs)) (cons '-> funs) (first funs))
         rst (assoc cfg :subs subs :lvl lvl)]
     `(  into ~rst ~func))))

(defmacro defsch
  "Define and create a scheduler map from scheduler DSL by scs named with name. Note: name is a atom structure. It is simular with defn macro."
  [name scs]
  `(def ~name (atom (sch ~scs))))

(defn runsch
  "Recurrently run ':run' functions among all sub nodes and return a grant."
  [ts sc]
  (let [{:keys [run subs req]} sc  ;Parse sc map
        gnt (cond (not (map? sc)) (throw (Exception. (str "Schedule node |" sc "| is not a map format.")))
                  (and (nil? run) (nil? subs) (nil? req)) (throw (Exception. (str "Schedule node |" sc "| do not include :run, :subs and :req fields.")))
                  (and (not (nil? run)) (nil? subs)) (throw (Exception. (str "Schedule none-leaflet node |" sc "| has not :subs field.")))
                  (and (not (nil? subs)) (nil? run)) (throw (Exception. (str "Schedule none-leaflet node |" sc "| has not :run field.")))
                  (and (not (nil? run)) (not (fn? run))) (throw (Exception. (str "Schedule none-leaflet node |" sc "| should have a fn type of :run.")))
                  )
        req (if (neg? req) nil req)
        gnt (if (zero? req) sc)
        gnt (if (fn? run)
              (apply run ts sc subs)
              sc)
        ;Bypass priority and append ts
        gnt (cond (zero? (:req gnt)) (assoc gnt :ts ts)
                  (pos? req) (assoc gnt :req req)
                  :else (assoc gnt :ts ts))
        ]
    gnt))  ;Grant中不需要的域删除)
  ; (cond
  ;   (number? sc) {:pri sc}    ;若调度节点为<数字>，则把该数字的值当作grant返回
  ;   (contains? sc :req) (into sc {:pri (:req sc)})
  ;   ;若调度节点包含:run域（非叶子节点），则使用:run函数对子节点进行选择
  ;   (contains? sc :run) (apply (get sc :run) sc (get sc :subs))
  ;   :else (throw (Exception. (str sc " is not a valid request format."))))

(defn upsch
  [sc gnt]
  (let [curr-node ((get sc :update) sc gnt)
        index (get gnt :index)
        sub-node (get-in sc [:subs index])
        sub? (get sub-node :run)
        sub-sc (when sub? (upsch sub-node (get gnt :sub)))]
    (if sub?
      (assoc-in curr-node [:subs index] sub-sc)
      curr-node)))

(defn grant [sc rst-fmt gnt-default]
  (let [gnt-iter (fn gnt-iter [sc gnt]
                   (if-let [{:keys [subs index]} sc]
                     (gnt-iter (get subs index) (cons (dissoc sc :run :update :subs) gnt))
                     (cons (dissoc sc :run :update :subs) gnt)))
     
        gnt (if (pos? (:index sc))
              (gnt-iter sc [])
              gnt-default)
        gnt (if (= :HIRE rst-fmt)
              gnt
              (last gnt))
        ]
    gnt))

(defn schedule
  "Runs schedule request node 'sc' one time (call runsch function), update sc and return a grant map. 
  
  * parameters:
    - sc: Schedule tree root node atom.
    - ts<optional>: The run time from program start. It is usefull for shaper and any other time-based scheduler elements.
  * Return:
    Schedule result (Grant). It has two result format configured via rst-fmt parameter: 
    - Hierachy format (:HIER): A vector of schedule level result map and indexed with schedule level.
    - Leaflet map format (:LEAF,Default): Just leaflet schedule level result is returned. 
    Note: The :run, :update and :subs will be removed from result by schedule function.
    "
  ([sc] (schedule sc 0))
  ([sc ts] (schedule sc ts :LEAF))
  ([sc ts rst-fmt]
    (let [gnt (runsch @sc ts)]
      (swap! sc upsch gnt)
      (grant @sc rst-fmt []))))


(defn request
  "Modify the scheduler sc's request fields where hierachy is pos vector."
  ([sc req]
   (swap! sc [:subs]
      (map #(cond (number? %) {:req %}
                  (contains? % :req) %
                  :else (throw (Exception. (str "Invalid leaflet schedule request format.")))
            ) req)))
  ([sc pos req]
   (let [req (cond (number? req) {:req req}
                   (contains? req :req) req
                   :else (throw (Exception. (str "Invalid leaflet schedule request format."))))
         v []   ; (interpos :subs pos)
         v (cons :subs v)
         v (conj v :subs)]
     (swap! sc assoc-in v req))))

