(ns pdpgraph
  (:require [clojure.java.io :as io])
  (:use [clojure.tools.cli :only (cli)])
  (:use [clojure.string :only (split)])
  (:import (java.io File))
  (:import (javax.swing JFrame))
  (:import (java.util.regex Pattern))
  (:import (com.mxgraph.swing mxGraphComponent)
           (org.jgrapht.ext JGraphXAdapter)
           (org.jgrapht.graph ListenableDirectedGraph DirectedPseudograph DefaultListenableGraph DirectedMultigraph DefaultEdge)
           (com.mxgraph.layout mxCircleLayout))
  (:gen-class :main true))

(def *frame* nil)

(defn get-frame []
  (alter-var-root
   #'*frame*
   (fn [x]
     (if *frame*
       (.dispose *frame*))
     (JFrame. "PomDP")))
  *frame*)

(defn scrape [file key]
  (with-open [rdr (io/reader file)]
    (first
     (doall
     (remove
        nil?
        (map #(let [line %
                    regexp (str "^" key ":\\s*(.*)\\s*")
                    val (second (re-find (Pattern/compile regexp) line))]
                (if val (split val #"\s+")))
             (line-seq rdr)))))))

(defn parse-pg [pg-file]
  (with-open [rdr (io/reader pg-file)]
    (doall
     (map #(let [line %]
             (let [row (map #'read-string (split line #"\s+"))]
               (replace { (symbol "-") (first row) } row)))
          (line-seq rdr)))))

(defn setup-gui [fn-create-graph die-on-close args]
  (let [frame (get-frame)
        ;dg (ListenableDirectedGraph. DefaultEdge)
        dg (DefaultListenableGraph. (DirectedPseudograph. String))
        ;dg (DefaultListenableGraph. (DirectedMultigraph. String))
        adapt (JGraphXAdapter. dg)
        layout (mxCircleLayout. adapt 400)
        ;layout (com.mxgraph.layout.hierarchical.mxHierarchicalLayout. adapt)
        ]
    (-> (.getContentPane frame) (.add (mxGraphComponent. adapt)))
    (apply fn-create-graph (concat args (list dg)))
    (-> frame (.setVisible true))
    (-> frame (.setLocation 1400 0))
    (if die-on-close
      (-> frame (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))
    (-> layout (.execute (.getDefaultParent adapt)))
    (-> frame (.pack))
    frame))

(defn create-pg-map [pg-data]
  (into {}
   (map #(let [node %]
           { (first node)
            { :action (second node)
             :next (drop 2 node) } } )
        pg-data)))

(defn get-node-name [elt actions]
  (let [node (first elt)
        nd (second elt)
        action-idx (get nd :action)
        action (nth actions action-idx)]
    (str action node)))

(defn create-graph [pg states actions obs dg]
  (doseq [elt pg]
    (let [node (first elt)
          nd (second elt)
          action-idx (get nd :action)
          action (nth actions action-idx)
          node-name (get-node-name elt actions)
          next (get nd :next)]
      (-> dg (.addVertex node-name))))
  (doseq [j (range (count pg))]
    (let [elt (nth (vec pg) j)
          node (first elt)
          nd (second elt)
          action-idx (get nd :action)
          action (nth actions action-idx)
          node-name (get-node-name elt actions)
          next (get nd :next)]
      (doseq [i (range (count obs))]
        (let [next-name (get-node-name (list (nth next i)
                                             (get pg (nth next i))) actions)
              edge (nth obs i)]
          (println node-name "->" next-name " name=" edge)
          (-> dg (.addEdge node-name next-name (str edge j))))))))

(defn run-graph [pomdp pg-file die-on-close]
  (let [pg-data (parse-pg pg-file)
        pg (create-pg-map pg-data)
        states (scrape pomdp "states")
        actions (scrape pomdp "actions")
        observations (scrape pomdp "observations")]
    (setup-gui create-graph die-on-close (list pg states actions observations))
    pg))

(defn run [opts args]
  (run-graph (File. (:input-pomdp opts)) (File. (:pg opts)) true)
  (println "created graph"))
        
(defn -main
  [& args]
  (let [[opts args banner]
        (cli args
             ["-h" "--help" "Show help" :flag true :default false]
             ["-i" "--input-pomdp" "REQUIRED: the input .pomdp file"]
             ["-o" "--pg" "REQUIRED: output .pg file"]
             )]
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (if
        (and
         (:input-pomdp opts)
         (:pg opts))
      (do
        (println "")
        (run opts args))
      (println banner))))
