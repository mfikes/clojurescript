(ns cljs.array-access-test
  (:require-macros [cljs.array-access.helper :refer [squelching-console-warn]])
  (:require [cljs.test :refer [deftest is]]
            [cljs.array-access.alpha :as alpha]))

(deftest unchecked-arrays-file-scope-test
  (is (not (alpha/unchecked-arrays?))))

(deftest aget-test
  (is (thrown? js/Error (aget nil 1)))
  (is (nil? (aget #js {} 1)))
  (is (nil? (aget #js [] 0)))
  (is (nil? (aget #js [1] -1)))
  (is (nil? (aget #js [1] 1)))
  (is (== 1 (aget #js [1] "0")))
  (is (nil? (aget [1] 0)))
  (is (== 1 (aget #js [1] 0)))
  (is (== 1 (aget #js {:foo 1} "foo")))
  (is (nil? (aget #js [#js {}] 0 0)))
  (is (nil? (aget #js [#js []] 0 0)))
  (is (nil? (aget #js [#js [1]] 0 -1)))
  (is (nil? (aget #js [#js [1]] 0 1)))
  (is (== 1 (aget #js [#js [1]] 0 "0")))
  (is (== 1 (aget #js [#js [1]] 0 0))))

(deftest aset-test
  (is (thrown? js/Error (aset nil 1 "x")))
  (is (= "x" (aset #js {} 1 "x")))
  (is (= "x" (aset #js [] 0 "x")))
  (is (= "x" (aset #js [1] -1 "x")))
  (is (= "x" (aset #js [1] 1 "x")))
  (is (= "x" (aset #js [1] "0" "x")))
  (is (= "x" (aset [1] 0 "x")))
  (is (= "x" (aset #js [1] 0 "x")))
  (let [v #js [1]]
    (aset v 0 "x")
    (is (= "x" (aget v 0))))
  (let [v #js {:foo 1}]
    (aset v "foo" "x")
    (is (= "x" (aget v "foo"))))
  (is (= "x" (aset #js [#js {}] 0 0 "x")))
  (is (= "x" (aset #js [#js []] 0 0 "x")))
  (is (= "x" (aset #js [#js [1]] 0 -1 "x")))
  (is (= "x" (aset #js [#js [1]] 0 1 "x")))
  (is (= "x" (aset #js [#js [1]] 0 "0" "x")))
  (is (= "x" (aset #js [#js [1]] 0 0 "x")))
  (let [v #js [#js [1]]]
    (aset v 0 0 "x")
    (is (= "x" (aget v 0 0)))))

(deftest unchecked-aget-test
  (is (thrown? js/Error (unchecked-get nil 1)))
  (is (nil? (unchecked-get #js {} 1)))
  (is (nil? (unchecked-get #js [] 0)))
  (is (nil? (unchecked-get #js [1] -1)))
  (is (nil? (unchecked-get #js [1] 1)))
  (is (== 1 (unchecked-get #js [1] "0")))
  (is (nil? (unchecked-get [1] 0)))
  (is (== 1 (unchecked-get #js [1] 0)))
  (is (== 1 (unchecked-get #js {:foo 1} "foo"))))

(deftest unchecked-set-test
  (is (thrown? js/Error (unchecked-set nil 1 "x")))
  (is (= "x" (unchecked-set #js {} 1 "x")))
  (is (= "x" (unchecked-set #js [] 0 "x")))
  (is (= "x" (unchecked-set #js [1] -1 "x")))
  (is (= "x" (unchecked-set #js [1] 1 "x")))
  (is (= "x" (unchecked-set #js [1] "0" "x")))
  (is (= "x" (unchecked-set [1] 0 "x")))
  (is (= "x" (unchecked-set #js [1] 0 "x")))
  (let [v #js [1]]
    (unchecked-set v 0 "x")
    (is (= "x" (aget v 0))))
  (let [v #js {:foo 1}]
    (unchecked-set v "foo" "x")
    (is (= "x" (aget v "foo")))))

(deftest checked-aget-test
  (squelching-console-warn
    (is (thrown? js/Error (checked-aget nil 1)))
    (is (nil? (checked-aget #js {} 1)))
    (is (nil? (checked-aget #js [] 0)))
    (is (nil? (checked-aget #js [1] -1)))
    (is (nil? (checked-aget #js [1] 1)))
    (is (== 1 (checked-aget #js [1] "0")))
    (is (nil? (checked-aget [1] 0)))
    (is (== 1 (checked-aget #js [1] 0)))
    (is (== 1 (checked-aget #js {:foo 1} "foo")))
    (is (nil? (checked-aget #js [#js {}] 0 0)))
    (is (nil? (checked-aget #js [#js []] 0 0)))
    (is (nil? (checked-aget #js [#js [1]] 0 -1)))
    (is (nil? (checked-aget #js [#js [1]] 0 1)))
    (is (== 1 (checked-aget #js [#js [1]] 0 "0")))
    (is (== 1 (checked-aget #js [#js [1]] 0 0)))))

(deftest checked-aset-test
  (squelching-console-warn
    (is (thrown? js/Error (checked-aset nil 1 "x")))
    (is (= "x" (checked-aset #js {} 1 "x")))
    (is (= "x" (checked-aset #js [] 0 "x")))
    (is (= "x" (checked-aset #js [1] -1 "x")))
    (is (= "x" (checked-aset #js [1] 1 "x")))
    (is (= "x" (checked-aset #js [1] "0" "x")))
    (is (= "x" (checked-aset [1] 0 "x")))
    (is (= "x" (checked-aset #js [1] 0 "x")))
    (let [v #js [1]]
      (checked-aset v 0 "x")
      (is (= "x" (aget v 0))))
    (let [v #js {:foo 1}]
      (checked-aset v "foo" "x")
      (is (= "x" (aget v "foo"))))
    (is (= "x" (checked-aset #js [#js {}] 0 0 "x")))
    (is (= "x" (checked-aset #js [#js []] 0 0 "x")))
    (is (= "x" (checked-aset #js [#js [1]] 0 -1 "x")))
    (is (= "x" (checked-aset #js [#js [1]] 0 1 "x")))
    (is (= "x" (checked-aset #js [#js [1]] 0 "0" "x")))
    (is (= "x" (checked-aset #js [#js [1]] 0 0 "x")))
    (let [v #js [#js [1]]]
      (checked-aset v 0 0 "x")
      (is (= "x" (aget v 0 0))))))

(deftest checked-aget'-test
  (is (thrown? js/Error (checked-aget' nil 1)))
  (is (thrown? js/Error (checked-aget' #js {} 1)))
  (is (thrown? js/Error (checked-aget' #js [] 0)))
  (is (thrown? js/Error (checked-aget' #js [1] -1)))
  (is (thrown? js/Error (checked-aget' #js [1] 1)))
  (is (thrown? js/Error (checked-aget' #js [1] "0")))
  (is (thrown? js/Error (checked-aget' [1] 0)))
  (is (== 1 (checked-aget' #js [1] 0)))
  (is (thrown? js/Error (checked-aget' #js [#js {}] 0 0)))
  (is (thrown? js/Error (checked-aget' #js [#js []] 0 0)))
  (is (thrown? js/Error (checked-aget' #js [#js [1]] 0 -1)))
  (is (thrown? js/Error (checked-aget' #js [#js [1]] 0 1)))
  (is (thrown? js/Error (checked-aget' #js [#js [1]] 0 "0")))
  (is (== 1 (checked-aget' #js [#js [1]] 0 0))))

(deftest checked-aset'-test
  (is (thrown? js/Error (checked-aset' nil 1 "x")))
  (is (thrown? js/Error (checked-aset' #js {} 1 "x")))
  (is (thrown? js/Error (checked-aset' #js [] 0 "x")))
  (is (thrown? js/Error (checked-aset' #js [1] -1 "x")))
  (is (thrown? js/Error (checked-aset' #js [1] 1 "x")))
  (is (thrown? js/Error (checked-aset' #js [1] "0" "x")))
  (is (thrown? js/Error (checked-aset' [1] 0 "x")))
  (is (= "x" (checked-aset' #js [1] 0 "x")))
  (let [v #js [1]]
    (checked-aset' v 0 "x")
    (is (= "x" (aget v 0))))
  (is (thrown? js/Error (checked-aset' #js [#js {}] 0 0 "x")))
  (is (thrown? js/Error (checked-aset' #js [#js []] 0 0 "x")))
  (is (thrown? js/Error (checked-aset' #js [#js [1]] 0 -1 "x")))
  (is (thrown? js/Error (checked-aset' #js [#js [1]] 0 1 "x")))
  (is (thrown? js/Error (checked-aset' #js [#js [1]] 0 "0" "x")))
  (is (= "x" (checked-aset' #js [#js [1]] 0 0 "x")))
  (let [v #js [#js [1]]]
    (checked-aset' v 0 0 "x")
    (is (= "x" (aget v 0 0)))))
