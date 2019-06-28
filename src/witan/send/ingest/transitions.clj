(ns witan.send.ingest.transitions
  (:require [clojure.set :as cs]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as rfs]
            [witan.send.constants :as const]))

(def NONSEND (name const/non-send))

(defn lookup-years [census]
  (x/into {}
          (map (fn [{:keys [calendar-year anon-ref academic-year need setting]}]
                 {[anon-ref calendar-year]
                  {:academic-year academic-year
                   :need need
                   :setting setting}}))
          census))

(defn create-transition-1 [lookup-years {:keys [calendar-year anon-ref academic-year] :as r}]
  (x/into (sorted-map)
          (let [lookup-calendar-year (inc calendar-year)]
            (-> r
                (cs/rename-keys {:academic-year :academic-year-1
                                 :need :need-1
                                 :setting :setting-1})
                (merge (-> (get lookup-years [anon-ref lookup-calendar-year]
                                {:academic-year (inc academic-year)
                                 :need NONSEND
                                 :setting NONSEND})
                           (cs/rename-keys {:academic-year :academic-year-2
                                            :need :need-2
                                            :setting :setting-2})))
                (assoc :calendar-year calendar-year)))))

(defn create-transition-2 [lookup-years {:keys [calendar-year anon-ref academic-year] :as r}]
  (x/into (sorted-map)
          (let [transition-calendar-year (dec calendar-year)]
            (-> r
                (cs/rename-keys {:academic-year :academic-year-2
                                 :need :need-2
                                 :setting :setting-2})
                (merge (-> (get lookup-years [anon-ref transition-calendar-year]
                                {:academic-year (dec academic-year)
                                 :need NONSEND
                                 :setting NONSEND})
                           (cs/rename-keys {:academic-year :academic-year-1
                                            :need :need-1
                                            :setting :setting-1})))
                (assoc :calendar-year transition-calendar-year)))))

(defn create-transitions [min-cal-year max-cal-year lookup-years {:keys [calendar-year] :as census-record}]
  (cond
    (= calendar-year min-cal-year) [(create-transition-1 lookup-years census-record)]
    (= calendar-year max-cal-year) [(create-transition-2 lookup-years census-record)]
    :else [(create-transition-1 lookup-years census-record) (create-transition-2 lookup-years census-record)]))

(defn transitions [census]
  (let [min-cal-year (transduce (map :calendar-year) rfs/min census)
        max-cal-year (transduce (map :calendar-year) rfs/max census)
        lookup-years (lookup-years census)]
    (x/into #{}
            (comp
             (mapcat (partial create-transitions min-cal-year max-cal-year lookup-years)))
            census)))
