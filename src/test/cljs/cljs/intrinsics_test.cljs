;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.intrinsics-test
  (:require [cljs.test :refer [deftest is]]))

(deftest test-cljs-2886
  (is (zero? (count "")))
  (is (== 1 (count "a")))
  (is (zero? (count #js [])))
  (is (== 1 (count #js [1])))
  (is (zero? (count [])))
  (is (== 1 (count [1]))))

(deftest test-cljs-3127
  (is (= :a))
  (is (= :a :a))
  (is (= :a :a :a))
  (is (not (= :a :b)))
  (is (not (= :a 1)))
  (is (not (= 1 :a)))
  (is (= 'a))
  (is (= 'a 'a))
  (is (= 'a 'a 'a))
  (is (not (= 'a 'b)))
  (is (not (= 'a 1)))
  (is (not (= 1 'a)))
  (is (not (= 'a :a)))
  (is (not (= :a 'a))))
