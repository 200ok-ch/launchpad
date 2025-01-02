(ns app.core
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            ["js-yaml" :as yaml]
            [shadow.cljs.modern :refer [js-await]]
            [clojure.string :as str]
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

;;; plumping

(defn tag-expr->string [tag-expr]
  (letfn [(build-str [expr level]
            (cond
              (string? expr)
              expr

              (sequential? expr)
              (let [strs (map #(build-str % (inc level)) expr)
                    join-str (if (odd? level) " & " " | ") ; Changed even? to odd?
                    needs-parens? (and (> level 0)
                                     (> (count strs) 1)
                                     (not= (count expr) 1))]
                (cond-> (apply str (interpose join-str strs))
                  needs-parens? (as-> s (str "(" s ")"))))))]
    (build-str tag-expr 0)))

(defn make-tag-predicate [tag-expr]
  (letfn [(build-pred [expr level]
            (cond
              ;; Single tag (string)
              (string? expr)
              #(contains? (set %) expr)

              ;; Vector/List of expressions
              (sequential? expr)
              (let [preds (map #(build-pred % (inc level)) expr)
                    combine-fn (if (even? level) every? some)]
                #(combine-fn true? (map (fn [pred] (pred %)) preds)))))]
    (build-pred tag-expr 0)))

(defn normalize-tags [tags]
  (if (string? tags)
    (str/split tags #"\s+")
    tags))

(defn long-form-entry [x]
  (if (map? x)
    x
    {:label (str x)
     :tags (normalize-tags x)}))

(defn parse-navigator [n]
  (map #(if (string? %) (keyword %) %)
       (map #(if (re-matches #"\d+" %) (js/parseInt %) %)
            (str/split n #"\."))))

(defn apply-template [prop {:keys [col row templates tags] :as item+}]
  (if-let [template (prop (last (first (filter #(contains? (set tags) (name (first %))) templates))))]
    (->> (re-seq #"#\{([^}]+)\}" template)
         (map (fn [[k v]] [k (get-in item+ (parse-navigator v))]))
         (reduce (fn [t [k v]] (str/replace t k v)) template))
    "x"))

(defn prepare-item [templates col row item]
  (let [item (-> (long-form-entry item)
                 (update :tags normalize-tags))
        item+ (assoc item :col col :row row :templates templates)
        defaults {:url (apply-template :url item+)
                  :short (apply-template :short item+)}]
    (merge defaults item)))

;;; ui

(defui table-cell [{:keys [templates col row items]}]
  (let [pred (make-tag-predicate [(:tags col) (:tags row)])
        items (map (partial prepare-item templates col row) items)
        hits (filter (comp pred :tags) items)]
    ($ :ul
       (for [{:keys [url short]} hits]
         ($ :li {:key url}
            ($ :a {:href url} short))))))

(defui table []
  (let [[data loading] (uix/use-context data-context)]
    ($ :table
       ($ :thead
          ($ :tr
             ($ :th "")
             (for [column-condition (:columns data)]
               ($ :th {:key column-condition}
                  (tag-expr->string column-condition)))))
       ($ :tbody
          (for [row-condition (:rows data)]
            ($ :tr {:key row-condition}
               ($ :td (tag-expr->string row-condition))
               (for [column-condition (:columns data)]
                 ($ :td {:key column-condition}
                    ($ table-cell {:templates (:templates data)
                                   :col (long-form-entry column-condition)
                                   :row (long-form-entry row-condition)
                                   :items (:items data)})
                    ))))))))

(defui sections []
  (let [[data loading] (uix/use-context data-context)]
    (for [[section links] (:aside data)]
      ($ :div {:key (name section)}
         ($ :h2 (name section))
         ($ :ul
            (for [{:keys [title url]} links]
              ($ :li {:key title}
                 ($ :a {:href url} title))))))))

(defui app []
  ($ data-provider
     ($ :main
        ($ :h2 "launchpad")
        ($ table))
     ($ :aside
        ($ sections))))

;;; setup

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))
