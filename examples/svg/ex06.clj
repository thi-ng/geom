(ns thi.ng.geom.examples.svg.ex06
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.matrix :refer [M32]]
   [thi.ng.geom.line :as l]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.svg.adapter :as adapt]))

(def arrow (svg/arrow-head 15 0.2 false))

;; compose decorators
(def labeler
  (svg/comp-decorators
   (svg/arrow-head 10 0.25 true {:fill "red"})
   (svg/line-label {:fill "black" :stroke "none"})))

(->> (svg/svg
      {:width 300 :height 300 :font-family "Arial" :font-size 12}
      ;; option 1: use line-strip-decorated
      (svg/line-strip-decorated
       [[5 0] [5 295] [300 295]]
       arrow nil arrow
       {:stroke "blue"})
      ;; option 2: attach decorators as metadata
      (with-meta
        (l/linestrip2 [[10 290] [100 150] [200 200] [290 10]])
        {:stroke "red"
         :stroke-dasharray "5 5"
         :__segment labeler
         :__label ["Jan 2014" "Feb 2014" "Mar 2014"]}))
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "svgdemo06-decorators.svg"))
