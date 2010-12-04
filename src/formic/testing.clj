(ns formic.testing
  (:require
    [clojure.java.io :as io])
  (:import (java.io BufferedWriter FileWriter)))

(def outfile "agent.log")

(defn append-spit [file msg]
  (with-open [w (io/writer (io/file file) :append true)]
    (spit w msg)))

(defn log [wtr]
  (letfn [(write [state]
             (append-spit outfile
               (str "Agent " (:id state) ":[" (:numwrites state) "]\n"))
            (assoc state :numwrites (inc (:numwrites state))))]
      (send wtr write)))

(def num-agents 10)
(def num-writes 50)


(defn start-this-mother-up []
  (let [agents (vec (map #(agent (-> {} (assoc :id %) (assoc :numwrites 0)))
                   (range num-agents)))]
    (dotimes [i num-writes]
      (let [idx (rand-int num-agents)
            agt (agents idx)]
        (println (str i "th run"))
        (log agt)))
    ))

