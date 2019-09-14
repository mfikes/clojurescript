;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.closure-fn-test
  (:require [cljs.test :refer-macros [deftest is]]
            [goog.object :as gobj]))

;;; The tests in this namespace verify the ability to properly invoke functions in
;;; closure libraries.

(deftest test-cljs-3165
  (is (= 1 (gobj/get #js {:a 1} "a")))
  (is (= 1 (gobj/get #js {:a 1} "a" 2)))
  (is (= 2 (gobj/get #js {:a 1} "b" 2))))
