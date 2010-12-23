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

(deftest call-clojure-from-js
  (letfn [(cljSquare [x] (* x x))]
    (is (= 25
           (execute-code
             (str "function performAction() {"
                    "return this.cljSquare(5);"
                  "}")
             [#'cljSquare])))))

