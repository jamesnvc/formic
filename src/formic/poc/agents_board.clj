(ns formic.poc.agents-board
  (:import (java.awt Color Graphics RenderingHints Graphics2D)
           (java.awt.image BufferedImage)
           (java.awt.event MouseListener MouseMotionListener)
           (javax.swing JFrame JPanel)))

(def len-x 10)
(def len-y 10)

(def num-agents 5)
(def remaining-agents (atom num-agents))
(def agents (atom []))


(defn make-board [x y]
  (vec
    (for [_ (range y)]
      (vec
        (for [_ (range x)]
          (ref nil))))))

(def *board* (make-board len-x len-y))

(defn at-loc
  ([x y]
    (at-loc *board* x y))
  ([board x y] ((board y) x)))

(defn agent-die [state]
  (println (:id state) ": Alas!  I am slain!" \newline
      (swap! remaining-agents dec) " agents remaining")
  (assoc state :alive false))

(defn kill [agt]
  (remove-watch agt :stepper)
  (send agt agent-die))

(defn rand-dir []
  ([inc dec identity] (rand-int 3)))

(defn agent-fn [state]
  (let [new-x (mod ((rand-dir) (:x state)) len-x)
        new-y (mod ((rand-dir) (:y state)) len-y)
        next-cell (at-loc new-x new-y)]
    (letfn [(move-into [cell]
              (when (and cell
                         (not= (:id @cell) (:id state))
                         (:alive @cell))
                (kill cell)
                (println "There can only be one!" \newline
                         (:id state) " killed " (:id @cell)
                         " at " new-x " " new-y))
               *agent*)]
    (when (:alive state)
      (dosync
        (ref-set (at-loc (:x state) (:y state)) nil)
        (alter next-cell move-into))
      (println (str (:id state) " moving to " new-x " " new-y))
      (assoc state :x new-x :y new-y)))))

(defn create-agent [id x y]
  (let [new-agent (agent {:id id :x x :y y :alive true})]
    (add-watch new-agent :stepper
               (fn [_ agt _ _]
                 (Thread/sleep (rand-int 3000))
                 (send agt agent-fn)))
    (dosync (ref-set (at-loc x y) new-agent))
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
       (when (<= ~'i w#)
         (loop [~'j (int 0)]
           (when (<= ~'j h#)
             (do ~@body)
             (recur (inc ~'j))))
           (recur (inc ~'i))))))

(defn stop-ants []
  (doall
    (for [agt @agents]
      (when (:alive @agt)
        (send agt agent-die))))
  nil)

(defn start-this-mother-up []
  (reset! agents (map #(create-agent % (next-x) (next-y))
                       (range num-agents)))
  (pmap #(send-off % agent-fn) @agents)
  (while (> @remaining-agents 1)
    (doall
      (for [agt @agents]
        (when-let [err (agent-error agt)]
          (println "Error in " (:id @agt))
          (.printStackTrace err)
          (restart-agent agt (assoc @agt :alive false) :clear-actions true))))
    (Thread/sleep 500))
  (println "Victory to "
           (:id (first (filter :alive (map deref @agents)))) "!"))

;; Running utilities

(def running (atom true))

(defmacro defworker
  "Defines a worker which discontinues looping his main body
   if the global atom 'running' is non-true. Exceptions are
   caught and printed"
  [name args & body]
  `(defn ~name ~args
     (while @running
      (try
        ~@body
      (catch Exception e#
        (-> e# .getMessage println))))))

;; Curses stuff

(defworker print-board []
  (loop [y 0]
    (when (< y len-y)
      (print "[")
      (loop [x 0]
        (when (< x len-x)
          (print " ")
          (if-let [contents (deref (at-loc x y))]
            (print (:id @contents))
            (print "."))
          (print " ")
          (recur (inc x))))
      (println "]")
      (recur (inc y))))
  (dotimes [_ 10]
    (print \newline))
  (Thread/sleep 500))


(defn show-in-term []
  (reset! running true)
  (future (binding [*out* nil]
            (start-this-mother-up)))
  (future (print-board)))

;; GUI stuff

(defn render-scene [#^Graphics2D g w h scale]
  (doto g
    (.setColor Color/RED)
    (.fillRect 0 0 (* scale (+ 2 w)) (* scale (+ 2 h))))
  (do-board [w h]
    (if-let [agt (deref (at-loc i j))]
      (do
        (if (:alive @agt)
          (.setColor g Color/WHITE)
          (.setColor g Color/GRAY))
        (.drawString g (str (:id @agt)) (* (inc i) scale) (* (inc j) scale)))
      (.setColor g Color/BLACK))
    (.fillRect g (* (inc i) scale) (* (inc j) scale) scale scale)))


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
                              (reset! running false))))
      (.addMouseListener (proxy [java.awt.event.MouseListener] []
                           (mouseExited     [e] nil)
                           (mouseEntered    [e] nil)
                           (mouseReleased   [e] nil)
                           (mousePressed    [e] nil)
                           (mouseClicked [e]
                            (.repaint panel))))
      (.setSize (* scale (+ 2 w)) (* scale (+ h 2)))
      (.add panel)
      .pack
      .show
      (.setVisible true))
    (future (render panel 20))
    (future (start-this-mother-up))))
