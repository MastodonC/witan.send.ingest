(ns witan.send.ingest.chart
  (:require [clojure.string :as s]
            [net.cgrand.xforms :as x]
            [cljplot.render :as plotr]
            [cljplot.build :as plotb]
            [cljplot.core :as plot]
            [clojure2d.color :as color]
            [witan.send.ingest :as i]
            [witan.send.ingest.transitions :as it]))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
(defn aggregate-by-domain [domain-key
                           sub-domain-xf sub-domain-key sub-domain-aggregate-xf
                           data]
  (let [sub-domain (into (sorted-set)
                         sub-domain-xf
                         data)]
    (->> (into (sorted-map)
               (x/by-key domain-key
                         (comp
                          (x/by-key sub-domain-key sub-domain-aggregate-xf)
                          (x/into (into (sorted-map)
                                        (map #(vector % 0)
                                             sub-domain)))))
               data)
         (reduce-kv (fn mapvals [a k v]
                      (assoc a k (mapv second v)))
                    (sorted-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charts

;; {need/setting [y1 y2 y3 y4]
;;  ,,,}

(defn grouped-column
  [title domain colours data]
  (let [palette (map colours domain)
        legend-spec (mapv
                     (fn [sd p]
                       [:rect sd {:color p}])
                     domain palette)]
    (-> (plotb/series [:stack-vertical [:bar data {:palette palette}]])
        (plotb/preprocess-series)
        (plotb/update-scale :y :ticks 20)
        (plotb/update-scale :y :fmt "%,.0f")
        ;; (plotb/update-scale :x :fmt name)
        (plotb/add-axes :left)
        (plotb/add-axes :bottom)
        ;;(plotb/add-label :bottom domain-name)
        ;;(plotb/add-label :left range-name)
        (plotb/add-label :top title {:font-size 24 :font "Open Sans Bold" :margin 36})
        (plotb/add-legend "" legend-spec)
        (plotr/render-lattice {:width 800 :height 600}))))

(defn grouped-column-mini
  [title domain colours data]
  (let [palette (map colours domain)
        legend-spec (mapv
                     (fn [sd p]
                       [:rect sd {:color p}])
                     domain palette)]
    (-> (plotb/series [:stack-vertical [:bar data {:palette palette}]])
        (plotb/preprocess-series)
        (plotb/update-scale :y :ticks 5)
        (plotb/update-scale :y :fmt "%,.0f")
        ;; (plotb/update-scale :x :fmt name)
        (plotb/add-axes :left)
        (plotb/add-axes :bottom)
        ;;(plotb/add-label :bottom domain-name)
        ;;(plotb/add-label :left range-name)
        (plotb/add-label :top title {:font-size 12 :font "Open Sans" :margin 16})
        (plotb/add-legend "" legend-spec)
        (plotr/render-lattice {:width 300 :height 200}))))

(defn stacked-column
  [title domain colours data]
  (let [palette (map colours domain)
        legend-spec (mapv
                     (fn [sd p]
                       [:rect sd {:color p}])
                     domain palette)]
    (-> (plotb/series
         [:stack-vertical [:sbar data {:palette palette}]])
        (plotb/preprocess-series)
        (plotb/update-scale :x :scale [:bands {:padding-in 0.0 :padding-out 0.2}])
        (plotb/update-scale :y :ticks 5)
        (plotb/update-scale :y :fmt "%,.0f")
        (plotb/add-axes :left)
        (plotb/add-axes :bottom)
        (plotb/add-label :top title {:font-size 24 :font "Open Sans Bold" :margin 36})
        ;; (plotb/add-label :bottom domain-name)
        ;; (plotb/add-label :left range-name)
        (plotb/add-legend "" legend-spec)
        (plotr/render-lattice {:width 800 :height 600}))))

(defn stacked-column-mini
  [title domain colours data]
  (let [palette (map colours domain)
        legend-spec (mapv
                     (fn [sd p]
                       [:rect sd {:color p}])
                     domain palette)]
    (-> (plotb/series
         [:stack-vertical [:sbar data {:palette palette}]])
        (plotb/preprocess-series)
        (plotb/update-scale :x :scale [:bands {:padding-in 0.0 :padding-out 0.2}])
        (plotb/update-scale :y :ticks 10)
        (plotb/update-scale :y :fmt "%,.0f")
        (plotb/add-axes :left)
        (plotb/add-axes :bottom)
        (plotb/add-label :top title {:font-size 12 :font "Open Sans" :margin 16})
        ;; (plotb/add-label :bottom domain-name)
        ;; (plotb/add-label :left range-name)
        (plotb/add-legend "" legend-spec)
        (plotr/render-lattice {:width 300 :height 200}))))

(defn save
  [{:keys [chart prefix file-name] :as chart-spec}]
  (plot/save chart (str prefix file-name))
  chart-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domain charts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Census based charts

(defn stacked-column-data [domain domain-key census]
  (let [base-map (into (sorted-map)
                       (zipmap domain (repeat 0)))]
    (->> (group-by :calendar-year census)
         (into (sorted-map))
         (map-kv (fn [v] (->> v
                              (map domain-key)
                              frequencies
                              (merge base-map)
                              (into (sorted-map))
                              vals))))))

(defn total-population-per-calendar-year-broken-down-by-need
  [{:keys [needs needs-palette census] :as data}]
  (let [title "Total population per calendar year broken down by need"
        domain-key :need
        domain needs
        palette needs-palette]
    (merge data
           {:chart
            (stacked-column
             title
             domain
             palette
             (stacked-column-data domain domain-key census))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

(defn total-population-per-calendar-year-broken-down-by-setting
  [{:keys [settings settings-palette census] :as data}]
  (let [title "Total population per calendar year broken down by setting"
        domain-key :setting
        domain settings
        palette settings-palette]
    (merge data
           {:chart
            (stacked-column
             title
             domain
             palette
             (stacked-column-data domain domain-key census))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

(defn needs-by-calendar-year
  [{:keys [calendar-years calendar-years-palette census] :as data}]
  (let [title "Needs by calendar year"
        domain-key :need
        subdomain calendar-years
        subdomain-key :calendar-year
        subddomain-palette calendar-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subddomain-palette
             (->> (group-by domain-key census)
                  (into (sorted-map))
                  (map-kv (fn [v] (->> v
                                       (map subdomain-key)
                                       frequencies
                                       (merge (into (sorted-map)
                                                    (zipmap subdomain (repeat 0))))
                                       (into (sorted-map))
                                       vals)))))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))


(defn settings-by-calendar-year
  [{:keys [calendar-years calendar-years-palette census] :as data}]
  (let [title "Settings by calendar year"
        domain-key :setting
        subdomain calendar-years
        subdomain-key :calendar-year
        subddomain-palette calendar-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subddomain-palette
             (->> (group-by domain-key census)
                  (into (sorted-map))
                  (map-kv (fn [v] (->> v
                                       (map subdomain-key)
                                       frequencies
                                       (merge (into (sorted-map)
                                                    (zipmap subdomain (repeat 0))))
                                       (into (sorted-map))
                                       vals)))))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))


(defn total-population-per-calendar-year-by-academic-year
  [{:keys [calendar-years calendar-years-palette census] :as data}]
  (let [title "Total population per calendar year by academic year"
        domain-key :academic-year
        subdomain calendar-years
        subdomain-key :calendar-year
        subddomain-palette calendar-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subddomain-palette
             (->> (group-by domain-key census)
                  (into (sorted-map))
                  (map-kv (fn [v] (->> v
                                       (map subdomain-key)
                                       frequencies
                                       (merge (into (sorted-map)
                                                    (zipmap subdomain (repeat 0))))
                                       (into (sorted-map))
                                       vals)))))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))
(comment

  (defn fname
    [{:keys [calendar-years calendar-years-palette census] :as data}]
    (let [title
          domain-key
          subdomain calendar-years
          subdomain-key :calendar-year
          subddomain-palette calendar-years-palette]
      (merge data
             {:chart
              (grouped-column
               title
               subdomain
               subddomain-palette
               (->> (group-by domain-key census)
                    (into (sorted-map))
                    (map-kv (fn [v] (->> v
                                         (map subdomain-key)
                                         frequencies
                                         (merge (into (sorted-map)
                                                      (zipmap subdomain (repeat 0))))
                                         (into (sorted-map))
                                         vals)))))
              :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
              :title title})))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transitions based charts
(defn transition-year-factor [transition-years transitions]
  (map-kv
   (fn [v] (->> v
                (map :calendar-year)
                (map (fn [y] (str y "-" (inc y))))
                frequencies
                (merge (into (sorted-map)
                             (zipmap transition-years (repeat 0))))
                (into (sorted-map))
                vals))
   transitions))


(defn count-of-joiners-per-calendar-year-by-academic-year
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of joiners per calendar year by academic year"
        domain-key :academic-year-2
        filter-pred i/joiner?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

(defn count-of-joiners-per-calendar-year-by-need
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of joiners per calendar year by need"
        domain-key :need-2
        filter-pred i/joiner?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

(defn count-of-joiners-per-calendar-year-by-setting
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of joiners per calendar year by setting"
        domain-key :setting-2
        filter-pred i/joiner?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))


(defn count-of-leavers-per-calendar-year-by-academic-year
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of leavers per calendar year by academic year"
        domain-key :academic-year-1
        filter-pred i/leaver?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))


(defn count-of-leavers-per-calendar-year-by-need
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of leavers per calendar year by need"
        domain-key :need-1
        filter-pred i/leaver?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

(defn count-of-leavers-per-calendar-year-by-setting
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of leavers per calendar year by setting"
        domain-key :setting-1
        filter-pred i/leaver?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))


(defn count-of-movers-per-calendar-year-by-academic-year
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of movers per calendar year by academic year"
        domain-key :academic-year-1
        filter-pred i/mover?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))


(defn count-of-stayers-per-calendar-year-by-academic-year
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of stayers per calendar year by academic year"
        domain-key :academic-year-1
        filter-pred i/stayer?
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (filter filter-pred)
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

;; #nofilter
(defn count-of-all-transitions-by-type
  [{:keys [transition-years transition-years-palette transitions] :as data}]
  (let [title "Count of all transitions by type"
        domain-key i/transition-type
        subdomain transition-years
        subdomain-palette transition-years-palette]
    (merge data
           {:chart
            (grouped-column
             title
             subdomain
             subdomain-palette
             (->> transitions
                  (group-by domain-key)
                  (into (sorted-map))
                  (transition-year-factor transition-years)))
            :file-name (s/lower-case (str (s/replace title " " "_") ".png"))
            :title title})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all validation charts
(defn validation-charts [prefix census]
  (let [calendar-years (x/into (sorted-set) (map :calendar-year) census)
        settings (x/into (sorted-set) (map :setting) census)
        needs (x/into (sorted-set) (map :need) census)
        transition-years (->> calendar-years
                              (drop-last)
                              (map (fn [y] (str y "-" (inc y)))))
        data {:prefix prefix
              :census census
              :transitions (it/transitions census)
              :calendar-years calendar-years
              :settings settings
              :needs needs
              :transition-years transition-years
              :settings-palette (zipmap settings (color/palette-presets :tableau-20))
              :needs-palette (zipmap needs (reverse (color/palette-presets :tableau-20)))
              :calendar-years-palette (zipmap calendar-years (color/palette-presets :green-orange-teal))
              :transition-years-palette (zipmap transition-years (color/palette-presets :green-orange-teal))}]

    ((juxt
      total-population-per-calendar-year-broken-down-by-need
      total-population-per-calendar-year-broken-down-by-setting
      needs-by-calendar-year
      settings-by-calendar-year
      total-population-per-calendar-year-by-academic-year
      count-of-joiners-per-calendar-year-by-academic-year
      count-of-joiners-per-calendar-year-by-need
      count-of-joiners-per-calendar-year-by-setting
      count-of-leavers-per-calendar-year-by-academic-year
      count-of-leavers-per-calendar-year-by-need
      count-of-leavers-per-calendar-year-by-setting
      count-of-movers-per-calendar-year-by-academic-year
      count-of-stayers-per-calendar-year-by-academic-year
      count-of-all-transitions-by-type)
     data)))
