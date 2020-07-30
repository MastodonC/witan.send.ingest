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

