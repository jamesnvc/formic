(ns formic.poc.agents-board
  (:import (java.awt Color Graphics RenderingHints Graphics2D)
           (java.awt.image BufferedImage)
           (java.awt.event MouseListener MouseMotionListener)
           (javax.swing JFrame JPanel)))

(def len-x 10)
(def len-y 10)

(defn make-board [x y]
  (vec
    (for [_ (range y)]
      (vec
        (for [_ (range x)]
          (atom nil))))))

(def *board* (make-board len-x len-y))

(defn at-loc
  ([x y]
    (at-loc *board* x y))
  ([board x y] ((board y) x)))

(def num-agents 5)
(def remaining-agents (atom num-agents))
(def agents (atom []))

(defn rand-dir []
  ([inc dec] (rand-int 2)))

(defn agent-die [state]
  (println (str (:id state) ": Alas!  I am slain!"))
  (remove-watch *agent* :stepper)
  (swap! remaining-agents dec)
  (assoc state :alive false))

(defn agent-fn [state]
  (let [new-x (mod ((rand-dir) (:x state)) len-x)
        new-y (mod ((rand-dir) (:y state)) len-y)
        next-cell (at-loc new-x new-y)]
    (if-let [foe (deref next-cell)]
      (do
        (send foe agent-die)
        (println "There can only be one!")
        (println (str (:id state) " killed " (:id @foe)
                      " at " new-x ", " new-y))))
    (if (:alive state)
      (do
        (reset! next-cell *agent*)
        (println (str (:id state) " moving to " new-x " " new-y))
        (assoc state :x new-x :y new-y))
      (remove-watch *agent* :stepper))))

(defn create-agent [id x y]
  (let [new-agent (agent {:id id :x x :y y :alive true})]
    (add-watch new-agent :stepper
               (fn [_ agt _ _]
                 (Thread/sleep (rand-int 3000))
                 (send agt agent-fn)))
    new-agent))

(def next-x
  (let [x-stream (atom (shuffle (range len-x)))]
    (fn []
      (let [retval (first @x-stream)]
        (swap! x-stream next)
        retval))))

(def next-y
  (let [y-stream (atom (shuffle (range len-y)))]
    (fn []
      (let [retval (first @y-stream)]
        (swap! y-stream next)
        retval))))

(defmacro do-board [[w h] & body]
  `(let [w# (int (dec ~w))
         h# (int (dec ~h))]
     (loop [~'i (int 0)]
       (when (< ~'i w#)
         (loop [~'j (int 0)]
           (when (< ~'j h#)
             (do ~@body)
             (recur (inc ~'j))))
           (recur (inc ~'i))))))

(defn stop-ants []
  (doall
    (for [agt @agents]
      (send agt agent-die)))
  nil)

(defn start-this-mother-up []
  (reset! agents (map #(create-agent % (next-x) (next-y))
                       (range num-agents)))
  (pmap #(send-off % agent-fn) @agents)
  (while (> 1 @remaining-agents)
    (Thread/sleep 500))
  (println (str "Victory to " (:id (first (filter :alive @agents))) "!")))

;; GUI stuff

(defn render-scene [#^Graphics2D g w h scale]
  (doto g
    (.setColor Color/BLACK)
    (.fillRect 0 0 (* scale w) (* scale h)))
  (do-board [w h]
    (if-let [agt (deref (at-loc i j))]
      (do
        (.setColor g Color/WHITE)
        (.fillRect g (* i scale) (* j scale) scale scale)))))

(def running (atom true))

(defmacro defworker [name args & body]
  " Defines a worker which discontinues looping his main body
    if the global atom 'running' is non-true. Exceptions are
    caught and printed "
  `(defn ~name ~args
     (while @running
      (try
        ~@body
      (catch Exception e#
        (-> e# .getMessage println))))))

(defworker render [panel fps]
  (.repaint panel)
  (Thread/sleep (* 1000 (/ 1 fps))))

(defn main [scale [w h]]
  (reset! running true)
  (reset! remaining-agents num-agents)
  (let [panel (proxy [JPanel] []
                (paint [g] (render-scene g w h scale)))]
    (doto (JFrame. "War of the Ants")
      (.addWindowListener (proxy [java.awt.event.WindowAdapter] []
                            (windowClosing [_]
                              (stop-ants)
                              (reset! running false)
                              (reset! remaining-agents 1))))
      (.add panel)
      (.setSize (* scale w) (* scale h))
      .pack
      .show
      (.setVisible true))
    (future (render panel 20))
    (start-this-mother-up)))
