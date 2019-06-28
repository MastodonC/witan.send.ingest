(ns witan.send.ingest.excel-report
  (:require [witan.send.ingest.transitions :as it]
            [witan.send.ingest.validation :as v]
            [dk.ative.docjure.spreadsheet :as xl]))

(defn workbook
  "Given a map of :tab-name and data create a workboo with all the right
  sheets."
  [data-seq]
  (apply xl/create-workbook (mapcat #((juxt :tab-name :data) %) data-seq)))

(defn save [file-name wkbk]
  (xl/save-workbook-into-file! file-name wkbk))

(defn save-validation-workbook [prefix wkbk]
  (xl/save-workbook-into-file!
   (str prefix "validation-report.xlsx")
   wkbk))

(defn transitions-data [transitions]
  (into [["anon-ref" "calendar-year" "setting-1" "need-1" "academic-year-1" "setting-2" "need-2" "academic-year-2"]]
        (mapv (juxt :anon-ref :calendar-year :setting-1 :need-1 :academic-year-1 :setting-2 :need-2 :academic-year-2)
              transitions)))

(defn transitions->workbook [file-name tab-name transitions]
  (save file-name
        (workbook
         [{:tab-name tab-name
           :data (transitions-data transitions)}])))

(defn validation-report [census valid-states costs]
  (let [transitions (it/transitions census)
        header ["anon-ref" "calendar-year" "setting-1" "need-1" "academic-year-1" "setting-2" "need-2" "academic-year-2" "anomalies"]]
    (workbook
     [{:tab-name "Invalid Transitions"
       :data (into [header]
                   (comp
                    (keep identity)
                    (map (partial v/validate-transition valid-states))
                    (remove #(empty? (:anomalies %)))
                    (remove #(some #{:outside-of-send :does-not-advance-1-ay} (:anomalies %))) ;; get rid of things we don't model
                    (map (juxt :anon-ref :calendar-year :setting-1 :need-1 :academic-year-1 :setting-2 :need-2 :academic-year-2
                               (fn [x] (->> x
                                            :anomalies
                                            (map name)
                                            ((partial clojure.string/join " ")))))))
                   transitions)}
      {:tab-name "Does not advance 1 academic year"
       :data (into [header]
                   (comp
                    (keep identity)
                    (map (partial v/validate-transition valid-states))
                    (remove #(empty? (:anomalies %)))
                    (filter #(some #{:does-not-advance-1-ay} (:anomalies %))) ;; get rid of things we don't model
                    (map (juxt :anon-ref :calendar-year :setting-1 :need-1 :academic-year-1 :setting-2 :need-2 :academic-year-2
                               (fn [x] (->> x
                                            :anomalies
                                            (map name)
                                            ((partial clojure.string/join " ")))))))
                   transitions)}
      {:tab-name "Missing Costs"
       :data (into [["Need" "Setting"]]
                   (->> (v/valid-costs transitions costs)
                        :no-defined-cost
                        (remove (fn [[_ s]] (= "GFE" s)))
                        (sort-by (juxt first second))))}
      {:tab-name "Records with No Cost Defined"
       :data (transitions-data
              (v/records-with-missing-costs transitions costs))}])))
