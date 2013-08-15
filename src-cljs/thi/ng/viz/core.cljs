(ns thi.ng.viz.core
  (:require
   [thi.ng.geom.core :as g]
   [goog.dom :as dom]))

(defn make-canvas
  [& {:keys [width height parent]}]
  (let [canvas (dom/createDom
                "canvas"
                (js-obj "width" width "height" height))]
    (when parent
      (dom/appendChild (dom/getElement parent) canvas))
    canvas))

(defn context!
  [ctx & {:as opts}]
  (doseq [[k v] opts]
    (aset ctx (name k) v))
  ctx)

(defn canvas-context-2d
  [canvas & opts]
  (apply context! (.getContext canvas "2d") opts))

(defn draw-line
  [ctx [ax ay] [bx by]]
  (doto ctx
    (.beginPath)
    (.moveTo ax ay)
    (.lineTo bx by)
    (.stroke))
  ctx)

(defn rect
  [ctx {:keys [[px py] width height]}]
  (.fillRect ctx px py width height))

(defn text
  [ctx x y & txt]
  (.fillText ctx (apply str txt) x y))

(defn line-seg
  [ctx [ax ay] [bx by]]
  (doto ctx
    (.moveTo ax ay)
    (.lineTo bx by))
  ctx)

(defn linestrip
  [ctx [[px py] & more :as points]]
  (.beginPath ctx)
  (.moveTo ctx px py)
  (doseq [[px py] more]
    (.lineTo ctx px py))
  (.stroke ctx)
  ctx)
