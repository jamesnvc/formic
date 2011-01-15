(ns formic.poc.script-loading
  (:import (org.mozilla.javascript Context ScriptableObject
                                   NativeObject NativeJavaObject
                                   Function)))

(defn isNativeObject?
  [obj]
  (= (class obj) 'org.mozilla.javascript.NativeObject))


(defmulti process-js-value class)

(defmethod process-js-value org.mozilla.javascript.NativeObject
  [#^NativeObject obj]
  (into {}
        (map #(vector (keyword %) (process-js-value (.get obj % nil)))
             (.getIds obj))))

(defmethod process-js-value org.mozilla.javascript.NativeJavaObject
  [#^NativeJavaObject obj]
  (.unwrap obj))

(defmethod process-js-value :default
  [obj]
  obj)

(defn execute-code
  "Takes javascript code as a string and calls it by evaluating the
  performAction() function with no arguments."
  ([^String code]
   (execute-code code []))
  ([^String code add-to-scope]
    (let [cx (Context/enter)
          scope (.initStandardObjects cx nil true)
          input (Context/javaToJS code scope)
          script (str code "performAction();")]
      (try
        (ScriptableObject/putProperty scope "input" input)
        (doall
          (for [[prop val] add-to-scope]
            (let [func (proxy [Function] []
                         (call [cx scope thisObj args]
                           (apply val args)))]
              (ScriptableObject/putProperty scope prop
                                          (Context/javaToJS func scope)))))
        (let [result (.evaluateString cx scope script "<cmd>" 1 nil)]
          (process-js-value result))
      (finally (Context/exit))))))
