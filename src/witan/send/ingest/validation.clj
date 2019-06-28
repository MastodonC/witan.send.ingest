(ns witan.send.ingest.validation
  (:require [clojure.data :as d]
            [clojure.math.combinatorics :as combo]
            [witan.send.constants :as const]
            [witan.send.check-inputs :as ci]
            [witan.send.ingest :as i]))

;; Lots of these functions below should migrate into witan.send.check-inputs

(def NON-SEND (name const/non-send))

(defn valid-costs
  "Returns
    - which need setting pairs occur in the transitions file that are uncosted
    - which costs have no matching need setting pair in the transtions
    - which need settnigs pairs do have defined costs"
  [transitions costs]
  (let [trans-need-settings (into #{}
                                  (comp
                                   (mapcat (juxt (juxt :need-1 :setting-1)
                                                 (juxt :need-2 :setting-2)))
                                   (remove #(some (fn [x] (= NON-SEND x)) %)))
                                  transitions)
        defined-costs (into #{} (map (juxt :need :setting)) costs)
        [ndc nmns dc] (d/diff trans-need-settings defined-costs)]
    {:no-defined-cost ndc
     :no-matching-need-setting nmns
     :defined-cost dc}))

(defn duplicate-costs
  "Returns any duplicate need setting pair and the number of times it
  occurs"
  [costs]
  (->> costs
       (map (juxt :need :setting))
       frequencies
       (keep (fn [[pair count]]
               (when (< 1 count)
                 [pair count])))))

(defn records-with-missing-costs [transitions costs]
  (let [no-defined-costs (into (sorted-set)
                               (->> (valid-costs transitions costs)
                                    :no-defined-cost
                                    (remove (fn [[_ s]] (= "GFE" s))) ; FIXME as we don't have GFE costs yet
                                    ))]
    (into []
          (filter (fn [x] (or (no-defined-costs ((juxt :need-1 :setting-1) x))
                              (no-defined-costs ((juxt :need-2 :setting-2) x)))))
          transitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; valid states
(defn valid-states-for-setting [{:keys [needs setting min-academic-year max-academic-year]}]
  (into []
        (map vec)
        (combo/cartesian-product (range min-academic-year (inc max-academic-year))
                                 needs
                                 (vector setting))))

(def set-of-valid-states
  (memoize
   (fn [valid-states]
     (into #{}
           (mapcat valid-states-for-setting)
           valid-states))))

(def set-of-valid-setting-transitions
  (memoize
   (fn [valid-states]
     (into #{}
           (comp (mapcat (fn [{:keys [setting setting->setting]}]
                           (combo/cartesian-product (vector setting)
                                                    setting->setting)))
                 (map vec))
           valid-states))))

(defn valid-ay-need-setting? [valid-states ay need setting]
  (let [vs (set-of-valid-states valid-states)]
    (vs [ay need setting])))

(defn valid-transition? [valid-states {:keys [academic-year-1 need-1 setting-1
                                              academic-year-2 need-2 setting-2]
                                       :as transition}]
  (and ((complement i/outside-of-send?) transition)
       ((complement ci/miscoded-nonsend?) transition)
       (i/advances-one-ay? transition)  ; this is "valid" but we don't model it so we exclude it
       (or (i/joiner? transition)
           (valid-ay-need-setting? valid-states
                                   academic-year-1 need-1 setting-1))
       (or (i/leaver? transition)
           (valid-ay-need-setting? valid-states
                                   academic-year-2 need-2 setting-2))
       (or (i/joiner? transition)
           (i/leaver? transition)
           ((set-of-valid-setting-transitions valid-states) [setting-1 setting-2]))))

(defn ay-need-setting-1-valid? [valid-settings
                                {:keys [academic-year-1 need-1 setting-1]
                                 :as transition}]
  (when (or (i/joiner? transition)
            (valid-ay-need-setting? valid-settings
                                    academic-year-1 need-1 setting-1))
    transition))

(defn ay-need-setting-2-valid? [valid-settings
                                {:keys [academic-year-2 need-2 setting-2]
                                 :as transition}]
  (when (or (i/leaver? transition)
            (valid-ay-need-setting? valid-settings
                                    academic-year-2 need-2 setting-2))
    transition))

(defn valid-setting-transition? [valid-states
                                 {:keys [setting-1 setting-2]
                                  :as transition}]
  (or (i/joiner? transition)
      (i/leaver? transition)
      ((set-of-valid-setting-transitions valid-states) [setting-1 setting-2])))

(defn validate [entity predicate anomaly]
  (if (predicate entity)
    entity
    (update entity :anomalies conj anomaly)))

(defn validate-transition
  "Take a transition and return it or a map with the original data and a
  vector of all the reasons why it has failed validation under the
  key :anomalies"
  [valid-states transition]
  (reduce (fn [a [predicate anomaly]]
            (validate a predicate anomaly))
          transition
          [[(complement i/outside-of-send?) :outside-of-send]
           [(complement ci/miscoded-nonsend?) :miscoded-non-send]
           [i/advances-one-ay? :does-not-advance-1-ay]
           [(partial ay-need-setting-1-valid? valid-states) :ay-need-setting-1-invalid]
           [(partial ay-need-setting-2-valid? valid-states) :ay-need-setting-2-invalid]
           [(partial valid-setting-transition? valid-states) :invalid-setting-transition]]))
