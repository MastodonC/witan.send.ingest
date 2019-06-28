(ns witan.send.ingest
  (:require [net.cgrand.xforms :as x]
            [witan.send.constants :as const]))

(def NON-SEND (name const/non-send))

(defn ->int [x]
  (cond (int? x)
        x
        (double? x)
        (int x)
        (string? x)
        (int (Double/valueOf x))
        :else
        (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                        {:value x}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Where should these go?
(defn joiner? [{:keys [setting-1 need-1 setting-2 need-2] :as rec}]
  (when (and (= NON-SEND setting-1)
             (= NON-SEND need-1)
             (not= NON-SEND setting-2)
             (not= NON-SEND need-2))
    rec))

(defn leaver? [{:keys [setting-1 need-1 setting-2 need-2] :as rec}]
  (when (and (= NON-SEND setting-2)
             (= NON-SEND need-2)
             (not= NON-SEND setting-1)
             (not= NON-SEND need-1))
    rec))

(defn mover? [{:keys [setting-1 need-1 setting-2 need-2] :as rec}]
  (when (and (not (or (joiner? rec)
                      (leaver? rec)))
             (or (not= setting-1 setting-2)
                 (not= need-1 need-2)))
    rec))

(defn stayer? [{:keys [setting-1 need-1 setting-2 need-2] :as rec}]
  (when (and (not (or (joiner? rec)
                      (leaver? rec)))
             (and (= setting-1 setting-2)
                  (= need-1 need-2)))
    rec))

(defn outside-of-send? [transition]
  (every? #(= % NON-SEND)
          ((juxt :setting-1 :need-1 :setting-2 :need-2) transition)))


(defn transition-type [rec]
  (cond (joiner? rec) :joiner
        (leaver? rec) :leaver
        (stayer? rec) :stayer
        (mover? rec)  :mover))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn advances-one-ay? [{:keys [academic-year-1 academic-year-2] :as rec}]
  (when (= 1 (- academic-year-2  academic-year-1))
    rec))

(defn skips-one-or-more-years? [{:keys [academic-year-1 academic-year-2] :as rec}]
  (when (< 1 (- academic-year-2  academic-year-1))
    rec))

(defn held-back-year? [{:keys [academic-year-1 academic-year-2] :as rec}]
  (when (zero? (- academic-year-2  academic-year-1))
    rec))

(defn goes-back-one-or-more-years? [{:keys [academic-year-1 academic-year-2] :as rec}]
  (when (neg? (- academic-year-2  academic-year-1))
    rec))

(defn observed-settings [recs]
  (into (sorted-set)
        (mapcat (juxt :setting-1 :setting-2))
        recs))

(defn observed-needs [recs]
  (into (sorted-set)
        (mapcat (juxt :need-1 :need-2))
        recs))

(defn calendar-years [recs]
  (into (sorted-set)
        (map :calendar-year)
        recs))

(defn count-held-back-one-year [transition-recs]
  (x/count (filter held-back-year?) transition-recs))

(defn count-advances-one-year [transition-recs]
  (x/count (filter advances-one-ay?) transition-recs))

(defn count-skips-year [transition-recs]
  (x/count (filter skips-one-or-more-years?) transition-recs))

(defn count-goes-back-year [transition-recs]
  (x/count (filter goes-back-one-or-more-years?) transition-recs))
