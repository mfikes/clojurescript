;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cljs.js-deps-test
  (:use clojure.test)
  (:require [cljs.js-deps :as js-deps]))

(deftest missing-global-export-test
  (is (empty? (#'js-deps/non-provided-global-exports {:file "somedir"})))
  (is (empty? (#'js-deps/non-provided-global-exports {:file "abc"
                                                :provides   ["abc-lib"]})))
  (is (empty? (#'js-deps/non-provided-global-exports {:file     "abc"
                                                :provides       ["abc-lib"]
                                                :global-exports '{abc-lib def}})))
  (is (empty? (#'js-deps/non-provided-global-exports {:file     "abc"
                                                      :global-exports '{abc-lib def}})))
  (is (= '#{def-lib} (set (#'js-deps/non-provided-global-exports {:file     "abc"
                                                            :provides       ["abc-lib"]
                                                            :global-exports '{def-lib def}}))))
  (is (= '#{def-lib} (set (#'js-deps/non-provided-global-exports {:file     "abc"
                                                            :provides       ["abc-lib"]
                                                            :global-exports '{abc-lib abc
                                                                              def-lib def}}))))
  (is (= '#{def-lib another} (set (#'js-deps/non-provided-global-exports {:file     "abc"
                                                                    :provides       ["abc-lib"]
                                                                    :global-exports '{abc-lib abc
                                                                                      def-lib def
                                                                                      another x}}))))
  (is (= '#{some-ns/def-lib some-ns/another} (set (#'js-deps/non-provided-global-exports {:file           "abc"
                                                                                          :provides       ["some-ns/abc-lib"]
                                                                                          :global-exports '#:some-ns{abc-lib abc
                                                                                                                     def-lib def
                                                                                                                     another x}})))))
