(ns witan.send.ingest.reports
  (:require [witan.send.excel-report :as er]
            [witan.send.org-report :as or]
            [witan.send.chart :as chart]
            [witan.send.ingest.transitions :as it]
            [witan.send.ingest.valid-states :as v]))

(defn save [output-prefix census costs settings-map valid-states]
  (let [data {:invalid-transition-report (er/validation-report census valid-states costs)
              :validation-charts (chart/validation-charts output-prefix census)
              :transitions (it/transitions census)
              :census census
              :costs costs
              :valid-states valid-states}]
    (er/save-validation-workbook output-prefix (:invalid-transition-report data))
    (run! chart/save (:validation-charts data))
    (or/save-validation-report output-prefix (or/report settings-map data))
    (it/->csv output-prefix (:transitions data))
    (v/->csv output-prefix (:valid-states data))
    data))
