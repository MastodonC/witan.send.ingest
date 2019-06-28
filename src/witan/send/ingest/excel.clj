(ns witan.send.ingest.excel
  (:require [dk.ative.docjure.spreadsheet :as ss]))

(defn read-row [^org.apache.poi.ss.usermodel.Row row]
  (map ss/read-cell (ss/cell-seq row)))

(defn rows [file-name sheet-name]
  (let [row-seq (->> (ss/load-workbook file-name)
                     (ss/select-sheet sheet-name)
                     ss/row-seq)]
    (map read-row row-seq)))
