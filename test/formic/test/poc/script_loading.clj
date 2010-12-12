(ns formic.test.poc.script-loading
  (:use [formic.poc.script-loading] :reload-all)
  (:use [clojure.test]))

(deftest execute-code-test
  (is (= "Hello World!"
        (execute-code "function performAction() { return 'Hello World!'; }"))))

(deftest execute-code-and-parse
  (is (= 1
         (int (:a (object-to-map
               (execute-code "function performAction() { return {'a': 1, 'b': 2}; }"
                             )))))))
