(ns witan.send.ingest.chart-test
  (:require [witan.send.chart :as sut]
            [clojure.test :as t]
            [net.cgrand.xforms :as x]))

(t/deftest test-aggregate-by-domain
  (t/testing "Happy path"
    (t/is (= (sut/aggregate-by-domain :year
                                      (map :fruit) :fruit x/count
                                      [{:year 1 :fruit "apple"}
                                       {:year 1 :fruit "apple"}
                                       {:year 1 :fruit "pear"}
                                       {:year 2 :fruit "apple"}
                                       {:year 2 :fruit "pear"}
                                       {:year 2 :fruit "pear"}])
             {1 [2 1], 2 [1 2]}))))
