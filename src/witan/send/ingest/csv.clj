(ns witan.send.ingest.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn ->valid-states [output-prefix valid-states]
  (with-open [w (io/writer (str output-prefix "valid-states.csv"))]
    (let [header [:setting :setting-group :min-academic-year :max-academic-year :needs :setting->setting]]
      (csv/write-csv
       w
       (into [(mapv name header)]
             (comp
              (map (fn [m] (update m :needs #(if (s/includes? % ",")
                                               %
                                               (s/join "," %)))))
              (map (fn [m] (update m :setting->setting #(if (s/includes? % ",")
                                                          %
                                                          (s/join "," %)))))
              (map (apply juxt header)))
             valid-states)))))

(defn ->double [x]
  (cond (double? x)
        x
        (int? x)
        (double x)
        (string? x)
        (Double/valueOf x)
        :else
        (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                        {:value x}))))

(defn ->costs [output-prefix costs]
  (with-open [w (io/writer (str output-prefix "costs.csv"))]
    (let [header [:need :setting :cost]]
      (csv/write-csv
       w
       (into [(mapv name header)]
             (comp
              (map (fn [m] (update m :need str)))
              (map (fn [m] (update m :setting str)))
              (map (fn [m] (update m :cost ->double)))
              (map (apply juxt header)))
             costs)))))

(defn ->population [output-prefix population]
  (with-open [w (io/writer (str output-prefix "population.csv"))]
    (let [header [:calendar-year :academic-year :population]]
      (csv/write-csv w
                     (into [(mapv name header)]
                           (comp
                            (map (fn [m] (update m :calendar-year int)))
                            (map (fn [m] (update m :academic-year int)))
                            (map (fn [m] (update m :population ->double)))
                            (map (apply juxt header)))
                           (sort-by (juxt :calendar-year :academic-year) population))))))
