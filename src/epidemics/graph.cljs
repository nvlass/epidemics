(ns ^:figwheel-hooks epidemics.graph
  (:require [clojure.set :as clj.set]))

;; Model constructor
;; https://www.pnas.org/content/99/suppl_1/2566

;; build the graph, then collect to nodes and edges

(defn mod-n [n k]
  (cond (< k 0) (+ k n)
        (> k 0) (mod k n)
        :else k))

(defn neighbors-for [nodes node mean-degree]
  (let [rng (concat (range (- (/ mean-degree 2)) 0)
                    (range 1 (+ (/ mean-degree 2) 1)))]
    (->> rng
         (map #(+ node %))
         (map #(mod-n nodes %)))))

(defn neighbor-edges [nodes node mean-degree]
  (let [neighbs (neighbors-for nodes node mean-degree)]
    (into #{} (mapcat #(vector [node %] [% node])
                      neighbs))))


(defn graph0 [nodes mean-degree]
  {:nodes (into #{} (range nodes))
   :edges (into #{} (reduce #(clojure.set/union %1 %2)
                            #{}
                            (map #(neighbor-edges nodes % mean-degree)
                                 (range nodes))))})

(defn rightmost-connections [nodes node mean-degree]
  (->> (range 1 (+ (/ mean-degree 2) 1))
       (map #(+ node %))
       (map #(mod-n nodes %))))

(defn reconnect-node [graph node dest]
  (let [new-dest (->> graph
                      :edges
                      (remove #(= (first %) node))
                      (remove #(= (second %) node))
                      rand-nth
                      second)]
    (-> graph
        (update :edges #(disj % [node dest] [dest node]))
        (update :edges #(conj % [node new-dest] [new-dest node])))))

(defn reconnect-nodes [graph node mean-degree beta]
  (reduce (fn [g dest]
            (if (< (rand) beta)
              (reconnect-node g node dest)
              g))
          graph
          (rightmost-connections (count (:nodes graph))
                                 node mean-degree)))


(defn wattz-strogatz-probably
  "(Probably, didn't validate thoroughly) generate a
  Watts-Strogatz graph"
  [nodes mean-degree beta]
  (let [graph (graph0 nodes mean-degree)]
    (reduce (fn [g nd]
              (reconnect-nodes g nd mean-degree beta))
            graph
            (:nodes graph))))


(defn node-degree [graph node]
  (count (->> graph
              :edges
              (filter #(= (first %) node)))))



(defn epidemic-graph
  "start with a ws graph and update the nodes so as to
  store info about the simulation, and also be more
  `cytoscape-friendly`"
  [nodes mean-degree beta]
  (let [g (wattz-strogatz-probably nodes mean-degree beta)]
    ;; update nodes and add their state (initially subsceptible)
    (-> g 
        (update :nodes (fn [nodes]
                         (into {} (map (fn [x]
                                         (let [n (->> g
                                                      :edges
                                                      (filter #(= (first %) x))
                                                      (map second))]
                                           [x {:id x
                                               :deg (count n)
                                               :state "s"
                                               :next-state "s"
                                               :neighbors n}]))
                                       nodes))))
        (update :edges (fn [edges]
                         (map (fn [x]
                                (let [[f s] x]
                                  {:id (str f "-" s)
                                   :source f
                                   :target s}))
                              edges)))
        ;; assoc sets to hold the 3 states
        (assoc :s (into #{} (:nodes g))
               :i #{}
               :r #{}))))



(defn calc-infected-map
  "calculate the set that will be infected in the next step"
  [epi-graph prob days]
  ;; possible infections come from the neighbors of infected nodes
  (let [possible-victim-ids (mapcat #(-> epi-graph (get-in [:nodes %]) :neighbors)
                                    (:i epi-graph))
        ;; we don't select keys, because prob of infection depends on
        ;; number of infected neighbors
        possible-victim-lst (map (:nodes epi-graph) possible-victim-ids)]
    ;; for each possible victim, calculate the next state
    (->> possible-victim-lst
         ;; only bother checking subsceptibles
         (filter #(= (:state %) "s")) 
         ;; check whether infection occurs 
         (map (fn [x]
                (if (< (rand) prob)
                  (assoc x :next-state "i" :next-days days)
                  x)))
         ;; keep only the future infected individuals
         (filter #(= (:next-state %) "i"))
         ;; transform into a map for merging
         (map #(vector (:id %) %))
         (into {}))))


(defn step-infected-map
  "calculate the set of recovered in the next step"
  [epi-graph]
  (->> epi-graph
       :i
       (select-keys (:nodes epi-graph))
       ;; 
       (reduce-kv (fn [m k v]
                    ;; should probably have a :next-days as well
                    (assoc m k
                           (let [{:keys [days]} v]
                             (if (= days 1)
                               (assoc v :next-state "r" :next-days 0)
                               (assoc v :next-state "i" :next-days (- days 1))))))
                  {})))

(defn step [graph prob inf-days]
  (let [step-infected (step-infected-map graph)
        new-infected  (calc-infected-map graph prob inf-days)]
    (let [new (-> graph
                  ;; merge maps into nodes
                  (update :nodes #(merge %
                                         step-infected
                                         new-infected))
                  ;; update nodes to their next state
                  (update :nodes #(reduce-kv (fn [m k v]
                                               (assoc m k (assoc v
                                                                 :state (:next-state v)
                                                                 :days (:next-days v))))
                                             {}
                                             %)))]
      ;; update :i, :r
      (-> new
          (assoc :r (->> new
                         :nodes
                         vals
                         (filter #(= (:state %) "r"))
                         (map :id)))
          (assoc :i (->> new
                         :nodes
                         vals
                         (filter #(= (:state %) "i"))
                         (map :id)))))))

(defn infect-node [graph node days]
  (-> graph
      (update :i conj node) ; update infected
      (assoc-in [:nodes node :state] "i")
      (assoc-in [:nodes node :days] days)))


