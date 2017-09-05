(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.


# Introduce


"}
  backpressure.core)
  



;=============================================================================================================
; WRR scheduler
;=============================================================================================================

(defn dynamic-threshold 
"
# Usage
## Parameters
* space: The size of total shared space
* cnt: The used quantity in shared space
* alpha: Shared coeffict
* max-thd(optional): Max limited threshold.
* coeff(optional): An adjustment coefficiate with buffer occupatation.
"
  ([space cnt alpha]
    (* alpha (- space cnt)))
  ([space cnt alpha max-thd]
    (min max-thd (* alpha (- space cnt))))
  ([space cnt alpha max-thd coeff]
    (min max-thd (* alpha (- space cnt) coeff))))
