;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.analyzer-api-tests
  (:require [cljs.analyzer.api :as ana-api])
  (:use clojure.test)
  (:import clojure.lang.ExceptionInfo))

(deftest cljs-warning-test
  (is (ana-api/warning-enabled? :undeclared-var)
      "Undeclared-var warning is enabled by default")
  (is (not (ana-api/no-warn (ana-api/warning-enabled? :undeclared-var)))
      "Disabled when all warnings are disabled"))

(def warning-form
  '(do (defn x [a b] (+ a b))
       (x 1 2 3 4)))

(defn warning-handler [counter]
  (fn [warning-type env extra]
    (when (ana-api/warning-enabled? warning-type)
      (swap! counter inc))))

(def test-cenv (atom {}))
(def test-env (ana-api/empty-env))

(deftest with-warning-handlers-test
  (let [counter (atom 0)]
    (ana-api/analyze test-cenv test-env warning-form nil
                     {:warning-handlers [(warning-handler counter)]})
    (is (= 1 @counter))))

(deftest vary-warning-handlers-test
  (let [counter (atom 0)]
    (cljs.analyzer/all-warn
      (ana-api/analyze test-cenv test-env warning-form nil
                       {:warning-handlers [(warning-handler counter)]}))
    (is (= 1 @counter))))

(defn resolve-handler1 [warning-type env extra]
  (println "H1" warning-type))

(defn resolve-handler2 [warning-type env extra]
  (println "H2" warning-type))

(deftest with-resolve-warning-handlers-test
  (is (= "H1 :fn-arity\nH2 :fn-arity\n"
         (with-out-str
           (ana-api/analyze
            test-cenv test-env warning-form nil
            (clojure.edn/read-string
             "{:warning-handlers [cljs.analyzer-api-tests/resolve-handler1
                                  cljs.analyzer-api-tests/resolve-handler2]}"))))))

(deftest with-resolve-warning-handlers-misconfigure-test
  (is (thrown-with-msg? ExceptionInfo #":warning-handlers symbol unqualified-sym is not fully qualified"
                       (ana-api/analyze
                        test-cenv test-env warning-form nil
                        (clojure.edn/read-string
                         "{:warning-handlers [unqualified-sym]}"))))
  (is (thrown-with-msg? ExceptionInfo #"Cannot require namespace referred by :warning-handlers value wont-find-this-ns/sym"
                        (ana-api/analyze
                         test-cenv test-env warning-form nil
                         (clojure.edn/read-string
                          "{:warning-handlers [wont-find-this-ns/sym]}"))))
  (is (thrown-with-msg? ExceptionInfo #":warning-handlers symbol cljs.analyzer-api-tests/unfound-sym not found"
                        (ana-api/analyze
                         test-cenv test-env warning-form nil
                         (clojure.edn/read-string
                          "{:warning-handlers [cljs.analyzer-api-tests/unfound-sym]}")))))
