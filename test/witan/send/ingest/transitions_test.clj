(ns witan.send.ingest.transitions-test
  (:require [witan.send.ingest.transitions :as sut]
            [witan.send.ingest :as i]
            [clojure.test :as t]))

(def mini-census
  [{:anon-ref 2620, :calendar-year 2018, :setting "SA", :need "N1", :academic-year 3}
   {:anon-ref 2620, :calendar-year 2017, :setting "SB", :need "N2", :academic-year 2}
   {:anon-ref 2620, :calendar-year 2016, :setting "SA", :need "N1", :academic-year 1}
   {:anon-ref 0, :calendar-year 2015, :setting "NONSEND", :need "NONSEND", :academic-year 1} ; dummy for min year
   {:anon-ref 0, :calendar-year 2019, :setting "NONSEND", :need "NONSEND", :academic-year 1} ; dummy for max year
   ])

(def lookup-years (sut/lookup-years mini-census))

(t/deftest lookup-test
  (t/testing "Testing lookup creation"
    (t/is (= (sut/lookup-years mini-census)
             {[2620 2018] {:academic-year 3, :need "N1", :setting "SA"},
              [2620 2017] {:academic-year 2, :need "N2", :setting "SB"},
              [2620 2016] {:academic-year 1, :need "N1", :setting "SA"},
              [0 2015] {:academic-year 1, :need "NONSEND", :setting "NONSEND"},
              [0 2019] {:academic-year 1, :need "NONSEND", :setting "NONSEND"}}))))

(t/deftest test-transition-generation
  (t/testing "Create leaver"
    (t/is (= [{:academic-year-1 3, :academic-year-2 4,
               :anon-ref 2620, :calendar-year 2018,
               :need-1 "N1", :need-2 "NONSEND",
               :setting-1 "SA", :setting-2 "NONSEND"}
              {:academic-year-1 2, :academic-year-2 3,
               :anon-ref 2620, :calendar-year 2017,
               :need-1 "N2", :need-2 "N1",
               :setting-1 "SB", :setting-2 "SA"}]
             (sut/create-transitions
              2015 2019
              lookup-years
              {:anon-ref 2620, :calendar-year 2018, :setting "SA", :need "N1", :academic-year 3}))))
  (t/testing "Create mover"
    (t/is (= [{:academic-year-1 2, :academic-year-2 3,
               :anon-ref 2620, :calendar-year 2017,
               :need-1 "N2", :need-2 "N1",
               :setting-1 "SB", :setting-2 "SA"}
              {:academic-year-1 1, :academic-year-2 2,
               :anon-ref 2620, :calendar-year 2016,
               :need-1 "N1", :need-2 "N2",
               :setting-1 "SA", :setting-2 "SB"}]
             (sut/create-transitions
              2015 2019
              lookup-years
              {:anon-ref 2620, :calendar-year 2017, :setting "SB", :need "N2", :academic-year 2}))))
  (t/testing "Create Joiner"
    (t/is (= [{:academic-year-1 1, :academic-year-2 2,
               :anon-ref 2620, :calendar-year 2016,
               :need-1 "N1", :need-2 "N2",
               :setting-1 "SA", :setting-2 "SB"}
              {:academic-year-1 0, :academic-year-2 1,
               :anon-ref 2620, :calendar-year 2015,
               :need-1 "NONSEND", :need-2 "N1",
               :setting-1 "NONSEND", :setting-2 "SA"}]
             (sut/create-transitions
              2015 2019
              lookup-years
              {:anon-ref 2620, :calendar-year 2016, :setting "SA", :need "N1", :academic-year 1}))))
  (t/testing "Test create-transitions"
    (t/is (let [ts (->> (sut/transitions mini-census)
                        (sort-by (juxt :anon-ref :calendar-year))
                        (drop 2)
                        vec)]
            ((every-pred (fn [[x _ _ _]] (i/joiner? x))
                         (fn [[_ x _ _]] (i/mover? x))
                         (fn [[_ _ x _]] (i/mover? x))
                         (fn [[_ _ _ x]] (i/leaver? x)))
             ts)))))
