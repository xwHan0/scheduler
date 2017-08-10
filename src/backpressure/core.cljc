(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.


# Introduce


"}
  backpressure.core)
  



;=============================================================================================================
; WRR scheduler
;=============================================================================================================

(defn dynamic-threshold [space cnt alpha]
  (* alpha (- space cnt)))

(defn high-low-threshold [cnt high-thd low-thd-ofst status]
  (if (>= cnt high-thd)
    true
    (let [low-thd (- high-thd low-thd-ofst)
          low-thd (if (neg? low-thd) 0 low-thd)]
      (if (<= cnt low-thd)
        false
        status))))