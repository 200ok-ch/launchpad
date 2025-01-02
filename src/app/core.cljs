(ns app.core
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            ["js-yaml" :as yaml]
            [shadow.cljs.modern :refer [js-await]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [go <!]]
            [cljs.core.async.interop :refer-macros [<p!]]))

;;; data

(def data-context (uix.core/create-context []))

(defn deserialize [string]
  (try
    (js->clj (yaml/load string) :keywordize-keys true)
    (catch js/Object e
      [:error (js->clj e)])))

(defui data-provider [{:keys [children]}]
  (let [[data set-data!] (uix/use-state nil)
        [loading set-loading!] (uix/use-state true)]
    (uix/use-effect
     #(go
        (let [request (<! (http/get "config.yml"))]
          (set-data! (deserialize (:body request)))
          (set-loading! false)))
     [])
    ($ (.-Provider data-context) {:value [data loading]}
       children)))

;;; ui

(defui x []
  (let [[data loading] (uix/use-context data-context)]
    ($ :div (prn-str data) (prn-str loading))))

(defui app []
  ($ data-provider
     ($ x)))

;;; setup

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))
