(ns scheduler.core-test
  (:require [clojure.test :refer :all]
            [scheduler.core :refer :all]))

(deftest sp-test
  (testing "Function Test."
    (is (= (sp {:req 1 :index 0} 
               {:req 1 :index 1}
               {:req 1 :index 2}
               {:req 1 :index 3}) 
           {:req 1 :index 0}))
    (is (= (sp {:req false :index 0} 
               {:req 1 :index 1}
               {:req 1 :index 2}
               {:req 1 :index 3}) 
           {:req 1 :index 1}))
    (is (= (sp {:req false :index 0} 
               {:req false :index 1}
               {:req true :index 2}
               {:req true :index 3}) 
           {:req true :index 2}))
  )
  
  (testing "Corner Test."
    (is (= (sp) 
           {:req false}))
    (is (= (sp {:req false :index 0} 
               {:req false :index 1}
               {:req false :index 2}
               {:req false :index 3}) 
           {:req false})))
  )

(deftest rr-test
  (testing "RR function Test."
    (is (= (rr 0
               {:req true :index 0}
               {:req true :index 1}
               {:req true :index 2}
               {:req true :index 3})
           {:req true :index 0})))

  (testing "Wrap Test."
    (let [rrf (wrap-rr)
          req [{:req true :index 0}
               {:req true :index 1}
               {:req true :index 2}
               {:req true :index 3}]]
      (for [times (range 6)]
        (is (= (apply rrf req)
               {:req true :index (mod times 4)})))))

  (testing "Wrap Corner Test."
    (let [rrf (wrap-rr)
          req [{:req false :index 0}
               {:req false :index 1}
               {:req false :index 2}
               {:req false :index 3}]]
      (is (= (rrf) {:req false}))
      (is (= (apply rrf req)) {:req false})))             
  )