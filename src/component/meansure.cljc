(ns
  ^{:author "xwHan"
    :doc "
"}
  component.meansure
  (:require [component.counter :as counter]))
  

(defn- parse-config [args]
  (into 
    {:min-val 0 :max-val 9999999999 :cir 1 :pir 1} 
    (if (map? (first args))
      (first args))
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
