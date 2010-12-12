(ns formic.poc.script-loading
  (:require
    [formic.poc.agents-board :as board])
  (:import (org.mozilla.javascript Context ScriptableObject
                                   NativeObject)))

(defn object-to-map
  [#^NativeObject obj]
  (into {}
        (map #(vector (keyword %) (.get obj % nil)) (.getIds obj))))

(defn execute-code
  "Takes javascript code as a string and calls it by evaluating the
  performAction() function with no arguments."
  [code]
  (let [cx (Context/enter)
        scope (.initStandardObjects cx)
        input (Context/javaToJS code scope)
        script (str code "performAction();")]
    (try
      (ScriptableObject/putProperty scope "input" input)
      (let [result (.evaluateString cx scope script
                                    "<cmd>" 1 nil)]
        result)
      (finally (Context/exit)))))
