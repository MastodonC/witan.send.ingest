(ns witan.send.ingest.validation-test
  (:require [witan.send.ingest.validation :as sut]
            [clojure.test :as t]))

(t/deftest test-duplicate-costs
  (t/testing "Happy path"
    (t/is (= (sut/duplicate-costs [{:need "foo" :setting "bar"}
                                   {:need "foo" :setting "bar"}
                                   {:need "foo" :setting "quux"}])
             '([["foo" "bar"] 2])))
    (t/is (= (sut/duplicate-costs [{:need "foo" :setting "bar"}
                                   {:need "foo" :setting "quux"}])
             '()))))

(def valid-states [{:setting "a"
                    :min-academic-year 0 :max-academic-year 5
                    :needs #{"1" "2" "3"}
                    :setting->setting #{"a" "b"}}
                   {:setting "b"
                    :min-academic-year 0 :max-academic-year 5
                    :needs #{"1" "2"}
                    :setting->setting #{"a" "b"}}
                   {:setting "c"
                    :min-academic-year 6 :max-academic-year 10
                    :needs #{"1" "2" "3"}
                    :setting->setting #{"c"}}])

(t/deftest test-validate-transition
  (t/testing "Happy path"
    (t/is (= (sut/validate-transition
              valid-states
              {:setting-1 "a",
               :need-1 "1",
               :academic-year-1 0,
               :setting-2 "a",
               :need-2 "1",
               :academic-year-2 1})
             {:setting-1 "a",
              :need-1 "1",
              :academic-year-1 0,
              :setting-2 "a",
              :need-2 "1",
              :academic-year-2 1}))
    (t/is (= (sut/validate-transition
              valid-states
              {:setting-1 "a",
               :need-1 "1",
               :academic-year-1 -1,
               :setting-2 "a",
               :need-2 "1",
               :academic-year-2 0})
             {:setting-1 "a",
              :need-1 "1",
              :academic-year-1 -1,
              :setting-2 "a",
              :need-2 "1",
              :academic-year-2 0,
              :anomalies '(:ay-need-setting-1-invalid)}))))
