(ns witan.send.ingest.org-report
  (:require [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [witan.send.ingest :as i]))

(defn h1 [s]
  (format "* %s\n\n" s))

(defn h2 [s]
  (format "** %s\n\n" s))

(defn link [url text]
  (format "[[%s][%s]]" url text))

(defn img [url]
  (format "[[%s]]" url))

(defn para [strs]
  (str (apply str strs) "\n\n"))

;; guts of this modified from clojure.pprint/print-table
(defn table [header-map ks rows]
  (when (seq rows)
    (let [widths (map
                  (fn [k]
                    (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                  ks)
          spacers (map #(apply str (repeat % "-")) widths)
          fmts (map #(str "%" % "s") widths)
          fmt-row (fn [leader divider trailer row]
                    (str leader
                         (apply str (interpose divider
                                               (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                 (format fmt (str col)))))
                         trailer))]
      (str
       "\n"
       (fmt-row "| " " | " " |" header-map)
       "\n"
       (fmt-row "|-" "-+-" "-|" (zipmap ks spacers))
       "\n"
       (apply str (interpose "\n" (mapv #(fmt-row "| " " | " " |" %) rows)))
       "\n\n"))))

(defn save-validation-report [prefix txt]
  (with-open [w (io/writer (str prefix "report.org"))]
    (.write w txt)))

(defn report [settings-map data]
  (str
   (h1 "Overview")

   (para ["Data was provided for calendar years "
          (apply str (interpose ", " (i/calendar-years (:census data))))
          "."])

   (para ["Invalid transitions and need/setting pairs without costs can be found "
          (link "./invalid-transitions.xls" "here")
          "."])

   (h1 "Setting Mappings")

   (table
    {:la "LA Name" :mc "MC Name"}
    [:la :mc]
    (map (fn [[k v]] {:la k :mc v}) settings-map))

   (h1 "SEND population per year")
   (table
    {:year "Year" :count "Count"}
    [:year :count]
    (map (fn [[k v ]] {:year k :count v})
         (into (sorted-map)
               (x/by-key :calendar-year x/count)
               (:census data))))

   (h1 "Transitions per year")
   (table
    {:year "Year" :count "Count"}
    [:year :count]
    (map (fn [[k v]] {:year (str k "-" (inc k)) :count v})
         (into (sorted-map)
               (x/by-key :calendar-year x/count)
               (:transitions data))))


   (h1 "Leavers per year")
   (table
    {:year "Year" :count "Count"}
    [:year :count]
    (map (fn [[k v]] {:year (str k "-" (inc k)) :count v})
         (into (sorted-map)
               (comp
                (filter i/leaver?)
                (x/by-key :calendar-year x/count))
               (:transitions data))))

   (h1 "Joiners per year")
   (table
    {:year "Year" :count "Count"}
    [:year :count]
    (map (fn [[k v]] {:year (str k "-" (inc k)) :count v})
         (into (sorted-map)
               (comp
                (filter i/joiner?)
                (x/by-key :calendar-year x/count))
               (:transitions data))))

   (h1 "Charts")
   (apply str
          (interpose "\n"
                     (map (fn [{:keys [title file-name]}]
                            (str (h2 title) (img (str "./" file-name))))
                          (:validation-charts data))))))
