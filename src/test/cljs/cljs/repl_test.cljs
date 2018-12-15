;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.repl-test
  (:require
   [cljs.repl]
   [cljs.test :refer [deftest is]]))

(deftest test-cljs-3017
  (let [m (cljs.repl/Error->map (js/TypeError.))]
    (is (= 'js/TypeError (get-in m [:via 0 :type])))))

(deftest test-cljs-3019
  (let [m (cljs.repl/Error->map (ex-info "" {}))]
    (is (= 'cljs.core/ExceptionInfo (get-in m [:via 0 :type])))))
