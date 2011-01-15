(ns formic.test.poc.script-loading
  (:use [formic.poc.script-loading] :reload-all)
  (:use [clojure.test]))

(deftest execute-code-test
  (is (= "Hello World!"
        (execute-code "function performAction() { return 'Hello World!'; }"))))

(deftest execute-code-and-parse
  (is (= 1
         (int (:a
               (execute-code (str "function performAction() { "
                                    "return {'a': 1, 'b': 2}; }")))))))

(deftest call-clojure-from-js
  (is (= 25
    (letfn [(cljSquare [x] (* x x))]
      (execute-code
        (str "function performAction() {"
               "return this.cljSquare(5);"
             "}")
        [["cljSquare" cljSquare]])))))

(deftest pass-in-object
  (is
    (= 5
       (letfn [(getVals [] {:a 5})]
         (execute-code (str "function performAction() { "
                              "var key = this.toKeyword('a');"
                              "return this.getVals().entryAt(key).nth(1);"
                            "}")
                  [["getVals" getVals]
                   ["toKeyword" keyword]])))))


