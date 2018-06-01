(require '[thi.ng.geom.viz.core :as viz] :reload)
(require '[thi.ng.geom.svg.core :as svg])
(require '[thi.ng.geom.vector :as v])
(require '[thi.ng.color.core :as col])
(require '[thi.ng.math.core :as m :refer [PI TWO_PI]])
(require '[thi.ng.color.core :as col])
(import '[java.util Calendar GregorianCalendar])

(def items
  [{:title "toxiclibs"          :from #inst "2006-03" :to #inst "2013-06" :type :oss}
   {:title "thi.ng/geom"        :from #inst "2011-08" :to #inst "2015-10" :type :oss}
   {:title "thi.ng/trio"        :from #inst "2012-12" :to #inst "2015-06" :type :oss}
   {:title "thi.ng/fabric"      :from #inst "2014-12" :to #inst "2015-09" :type :oss}
   {:title "thi.ng/simplecl"    :from #inst "2012-10" :to #inst "2013-06" :type :oss}
   {:title "thi.ng/raymarchcl"  :from #inst "2013-02" :to #inst "2013-05" :type :oss}
   {:title "thi.ng/structgen"   :from #inst "2012-10" :to #inst "2013-02" :type :oss}
   {:title "thi.ng/luxor"       :from #inst "2013-10" :to #inst "2015-06" :type :oss}
   {:title "thi.ng/morphogen"   :from #inst "2014-03" :to #inst "2015-06" :type :oss}
   {:title "thi.ng/color"       :from #inst "2014-09" :to #inst "2015-10" :type :oss}
   {:title "thi.ng/validate"    :from #inst "2014-05" :to #inst "2015-06" :type :oss}
   {:title "thi.ng/ndarray"     :from #inst "2015-05" :to #inst "2015-06" :type :oss}
   {:title "thi.ng/tweeny"      :from #inst "2013-10" :to #inst "2015-01" :type :oss}
   {:title "Co(De)Factory"      :from #inst "2013-12" :to #inst "2014-08" :type :project}
   {:title "Chrome WebLab"      :from #inst "2011-05" :to #inst "2012-11" :type :project}
   {:title "ODI"                :from #inst "2013-07" :to #inst "2013-10" :type :project}
   {:title "LCOM"               :from #inst "2012-06" :to #inst "2013-05" :type :project}
   {:title "V&amp;A Ornamental" :from #inst "2010-12" :to #inst "2011-05" :type :project}
   {:title "Engine26"           :from #inst "2010-08" :to #inst "2010-12" :type :project}
   {:title "Resonate"           :from #inst "2012-04" :to #inst "2012-04" :type :workshop}
   {:title "Resonate"           :from #inst "2013-03" :to #inst "2013-03" :type :workshop}
   {:title "Resonate"           :from #inst "2014-04" :to #inst "2014-04" :type :workshop}
   {:title "Resonate"           :from #inst "2015-04" :to #inst "2015-04" :type :workshop}
   {:title "Resonate"           :from #inst "2012-04" :to #inst "2012-04" :type :talk}
   {:title "Resonate"           :from #inst "2013-03" :to #inst "2013-03" :type :talk}
   {:title "Resonate"           :from #inst "2014-04" :to #inst "2014-04" :type :talk}
   {:title "Resonate"           :from #inst "2015-04" :to #inst "2015-04" :type :talk}
   {:title "Retune"             :from #inst "2014-09" :to #inst "2014-09" :type :talk}
   {:title "Bezalel"            :from #inst "2011-04" :to #inst "2011-04" :type :workshop}
   {:title "V&amp;A"            :from #inst "2011-01" :to #inst "2011-03" :type :workshop}
   {:title "HEAD"               :from #inst "2010-10" :to #inst "2010-10" :type :workshop}
   {:title "ETH"                :from #inst "2010-11" :to #inst "2010-11" :type :workshop}
   {:title "SAC"                :from #inst "2012-11" :to #inst "2012-11" :type :workshop}
   {:title "SAC"                :from #inst "2014-12" :to #inst "2014-12" :type :workshop}
   {:title "MSA"                :from #inst "2013-04" :to #inst "2013-04" :type :workshop}
   {:title "Young Creators"     :from #inst "2014-06" :to #inst "2014-06" :type :workshop}
   {:title "EYEO"               :from #inst "2013-06" :to #inst "2013-06" :type :talk}
   {:title "Reasons"            :from #inst "2014-02" :to #inst "2014-02" :type :talk}
   {:title "Reasons"            :from #inst "2014-09" :to #inst "2014-09" :type :talk}])

(def item-type-colors {:project "#0af" :oss "#63f" :workshop "#9f0" :talk "#f9f"})

(def month (* (/ (+ (* 3 365) 366) 4.0 12.0) 24 60 60 1000))
(def year  (* month 12))

(defn ->epoch [^java.util.Date d] (.getTime d))

;; http://stackoverflow.com/questions/9001384/java-date-rounding
(defn round-to-year
  [epoch]
  (let [cal (GregorianCalendar.)]
    (doto cal
      (.setTimeInMillis (long epoch))
      (.add Calendar/MONTH 6)
      (.set Calendar/MONTH 0)
      (.set Calendar/DAY_OF_MONTH 1)
      (.set Calendar/HOUR 0)
      (.set Calendar/MINUTE 0)
      (.set Calendar/SECOND 0)
      (.set Calendar/MILLISECOND 0))
    (.get cal Calendar/YEAR)))

(defn make-gradient
  [[id base]]
  (let [base (col/as-hsva (col/css base))]
    (svg/linear-gradient
     id {} [0 base] [1 (col/adjust-saturation base -0.66)])))

(defn item-range [i] [(->epoch (:from i)) (->epoch (:to i))])

(defn timeline-spec
  [type offset]
  {:values     (if type (filter #(= type (:type %)) items) items)
   :offset     offset
   :item-range item-range
   :attribs    {:fill "white"
                :stroke "none"
                :font-family "Arial"
                :font-size 10}
   :shape      (viz/labeled-rect-horizontal
                {:h         14
                 :r         7
                 :min-width 30
                 :base-line 3
                 :label     :title
                 :fill      #(str "url(#" (name (:type %)) ")")})
   :layout     viz/svg-stacked-interval-plot})

;; Create stacked timeline with *all* items
(->> {:x-axis (viz/linear-axis
               {:domain [(->epoch #inst "2010-09") (->epoch #inst "2015-06")]
                :range  [10 950]
                :pos    160
                :major  year
                :minor  month
                :label  (viz/default-svg-label round-to-year)})
      :y-axis (viz/linear-axis
               {:domain  [0 9]
                :range   [10 160]
                :visible false})
      :grid   {:minor-x true}
      :data   [(timeline-spec nil 0)]}
     (viz/svg-plot2d-cartesian)
     (svg/svg
      {:width 960 :height 200}
      (apply svg/defs (map make-gradient item-type-colors)))
     (svg/serialize)
     (spit "out/timeline.svg"))

;; Create stacked timeline vertically grouped by item type
(->> {:x-axis (viz/linear-axis
               {:domain [(->epoch #inst "2010-09") (->epoch #inst "2015-06")]
                :range  [10 950]
                :pos    220
                :major  year
                :minor  month
                :label  (viz/default-svg-label round-to-year)})
      :y-axis (viz/linear-axis
               {:domain  [0 13]
                :range   [10 220]
                :visible false})
      :grid   {:minor-x true}
      :data   [(timeline-spec :project 0)
               (timeline-spec :oss 2)
               (timeline-spec :workshop 10)
               (timeline-spec :talk 11)]}
     (viz/svg-plot2d-cartesian)
     (svg/svg
      {:width 960 :height 245}
      (apply svg/defs (map make-gradient item-type-colors)))
     (svg/serialize)
     (spit "out/timeline-separate.svg"))
