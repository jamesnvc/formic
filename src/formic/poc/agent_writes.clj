(ns formic.poc.agent-writes
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

(def num-agents 3)
(def num-writes 5)

(defn start-this-mother-up []
  (let [agents (map #(agent (-> {} (assoc :id %) (assoc :numwrites 0)))
                   (range num-agents))]
    (letfn [(agent-write [agt]
              (dotimes [_ num-writes]
                (log agt)))]
      (pmap agent-write agents))))
