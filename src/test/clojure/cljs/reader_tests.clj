;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.reader-tests
  (:use clojure.test)
  (:require [cljs.tagged-literals :as tags])
  (:import (clojure.lang IObj)))

(deftest js-value-should-support-metadata
  ; CLJS-1898
  (let [js-tag-form (tags/->JSValue {})]
    (is (instance? IObj js-tag-form))
    (is (nil? (meta js-tag-form)))
    (is (= {:source "abc"} (meta (with-meta js-tag-form {:source "abc"}))))))
