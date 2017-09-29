(ns
  ^{:author "xwHan"
    :doc "Common Scheduler models for ESL in communication fields.


# Introduce


"}
  middleware.calendar)
  

(defn calendar
  ([handler])
  ([handler mname]
    {:run
      (fn [{:keys [mname] :as cfg} ts & nodes]
        (let [{:keys [calendar-tbl]} mname
              nodes (map (->> nodes vec) calendar-tbl)]
          (apply (:run handler) cfg ts nodes)))
     :update
      (fn [{:keys [] :as sc} gnt]
        ((get handler :update) sc gnt))
     }))

 
        

        
        
        