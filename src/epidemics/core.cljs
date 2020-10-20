(ns ^:figwheel-hooks epidemics.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [clojure.core.async :as a]
   [epidemics.graph :as g]))

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:graph     nil
                          :cy        nil
                          :nnodes    400
                          :degree    4
                          :beta      0.30
                          :prob      0.2
                          :inf-days  7
                          :days      0
                          :ctrl-chan (a/chan)}))

(defn get-app-element []
  (gdom/getElement "app"))

(def state-colors
  {"i" "red"
   "s" "green"
   "r" "black"})

(defn- key-path->id [key-path]
  (clojure.string/join "-" (map name key-path)))

(defn sim-param-entry [{:keys [key-path label placeholder
                               validator xform default]}]
  (let [id (key-path->id key-path)]
    [:div.column
     [:label {:htmlFor id} label]
     [:input {:id          id
              :type        "text"
              :value       (get-in @app-state key-path)
              :on-change   (fn [e]
                             (let [v (-> e .-target .-value)]
                               (swap! app-state assoc-in key-path v)))
              :on-blur     (fn [e]
                             (let [v  (-> e .-target .-value)
                                   vv (xform v)]
                               (swap! app-state assoc-in key-path
                                      (cond (empty? v)     v
                                            (validator vv) vv
                                            :else          default))))
              :placeholder placeholder}]]))


(defn update-node [x]
  (-> x
      (assoc :stateColor (-> x :state state-colors))
      (update :id #(str %))))

(defn load-graph [graph cy]
  (.add cy
        (clj->js (concat (map (fn [[_ x]] {:group "nodes"
                                           :data (update-node x)})
                              (:nodes graph))
                         (map (fn [x] {:group "edges"
                                       :data x})
                              (:edges graph))))))

(defn update-cy [{:keys [graph cy]}]
  (.batch cy (fn []
               (doseq [n (vals (:nodes graph))]
                 (let [q (update-node n)]
                   (.data (.$ cy (str "#" (:id q)))
                          (clj->js q)))))))


(defn layout-graph [cy]
  (.run (.layout cy (clj->js {:name "fcose"
                              :quality "default"
                              :randomize true
                              :packComponents true}))))


(def cs-style "
  node {height: data(deg); width: data(deg);}
  node {background-color: data(stateColor);}
  edge { width: 0.1; }
  ")

(defn init-graph []
  ;; UGLY UGLY
  (let [_          (swap! app-state assoc :loading true)
        {:keys
         [nnodes
          beta
          degree]} @app-state
        chan-res   (a/chan)]
    ;; do the work
    (a/go
      (let [graph (g/epidemic-graph nnodes degree beta)
            ccy   (js/cytoscape (clj->js {:container (gdom/getElement "grph")
                                          :style     cs-style}))]
        (load-graph graph ccy)
        (layout-graph ccy)
        (.on ccy "tap" "node"
             (fn [evt]
               (when-not (:is-running @app-state)
                 (let [node-id (-> evt .-target .id js/parseInt)]
                   (swap! app-state
                          update :graph #(g/infect-node % node-id (:inf-days @app-state)))
                   ;; ugly ugly
                   (update-cy @app-state)))))
        (a/>! chan-res {:graph graph :ccy ccy})))

    ;; update stuff when work is done
    (a/go
      (let [{:keys [graph ccy]} (a/<! chan-res)]
        (swap! app-state assoc :graph graph :cy ccy :loading false)))))

(defn statistics []
  (if (:loading @app-state)
    "LOADING GRAPH"
    [:table
     [:tbody
      [:tr
       [:td [:b "Days"]] [:td (:days @app-state)]
       [:td [:b "Infected"]] [:td (or
                                   (count (-> @app-state :graph :i))
                                   "-")]
       [:td [:b "Removed (dead / recovered)"]] [:td (or
                                                     (count (-> @app-state :graph :r))
                                                     "-")]]
      [:tr
       [:td {:colSpan 6} "Click on a node to place patient(s) zero"]]]]))


;; UGLY UGLY
(defn step-sim []
  (swap! app-state
         update :graph
         #(g/step % (:prob @app-state) (:inf-days @app-state)))
  (update-cy @app-state)
  (swap! app-state update :days inc))

(defn stop-sim []
  (swap! app-state assoc :is-running false))

(defn toggle-sim []
  (if-not (:is-running @app-state)
    ;; start and step
    (do
      (swap! app-state assoc :is-running true)
      (a/go-loop []
        (a/alt! (a/timeout 1000) ([_] (step-sim) (recur))
                (:ctrl-chan @app-state) ([_] (stop-sim)))))
    ;; stop
    (a/go
      (a/>! (:ctrl-chan @app-state) :stop))))

(defn epidemic-model []
  [:div.container {:style {:height "100%"}}
   [:h3 "Epidemic Spreading Simulation on a Graph"]
   [:h5 "See " [:a {:href "https://github.com/nvlass/epidemics"} "README"] " for model info"]
   [:form
    [:div.row
     [sim-param-entry {:key-path [:nnodes] 
                       :label "Number of individuals"
                       :placeholder ">= 100"
                       :validator #(and (not (js/isNaN %))
                                        (>= % 100))
                       :xform #(js/parseInt %)
                       :default 400}]
     [sim-param-entry {:key-path [:degree] 
                       :label "Degree"
                       :placeholder "even number"
                       :validator #(and (not (js/isNaN %))
                                        (even? %)
                                        (> % 0))
                       :xform #(js/parseInt %)
                       :default 4}]
     [sim-param-entry {:key-path [:beta] 
                       :label "Watts–Strogatz beta param"
                       :placeholder "0 <= beta <= 1"
                       :validator #(and (not (js/isNaN %))
                                        (<= 0 % 1.0))
                       :xform #(js/parseFloat %)
                       :default 0.5}]]
    [:div.row
     [sim-param-entry {:key-path [:prob] 
                       :label "Disease Transmission Probability"
                       :placeholder "0 <= p <= 1"
                       :validator #(and (not (js/isNaN %))
                                        (<= 0 % 1.0))
                       :xform #(js/parseFloat %)
                       :default 0.5}]
     [sim-param-entry {:key-path [:inf-days] 
                       :label "# Days an individual is infectious"
                       :placeholder ">= 1"
                       :validator #(and (not (js/isNaN %))
                                        (> % 1))
                       :xform #(js/parseInt %)
                       :default 7}]]]
   [:div.row
    [:div.column
     [:div.button.button-outline
      {:on-click init-graph
       :enabled (not (:is-running @app-state))}
      "Init graph"]]
    [:div.column
     [:div.button.button-outline
      {:on-click toggle-sim}
      (if-not (:is-running @app-state)
        "Start simulation"
        "Stop simulation")]]
    [:div.column
     [:div.button.button-outline
      {:on-click (fn []
                   (when-let [cy (:cy @app-state)]
                     (.fit cy)))}
      "Reset Zoom"]]]

   [:div.row
    [:div.column
     [statistics]]]
   
   [:div.row {:style {:width "100%"
                      :height "100%"}}
    [:div.column {:style {:width "80%"
                          :height "80%"}
                  :id "grph"}]]])

(defn mount [el]
  (rdom/render [epidemic-model] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
;; (defonce do-it-once-hack (mount-app-element))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

