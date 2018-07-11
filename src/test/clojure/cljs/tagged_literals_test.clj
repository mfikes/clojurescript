(ns cljs.tagged-literals-test
  (:require [cljs.tagged-literals :as tags])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream ObjectInputStream ObjectOutputStream)
           (cljs.tagged_literals JSValue))
  (:use clojure.test))

(defn obj->bytes [obj]
  (let [bytestream (ByteArrayOutputStream.)]
    (doto (ObjectOutputStream. bytestream) (.writeObject obj))
    (.toByteArray bytestream)))

(defn bytes->obj [bs]
  (.readObject (ObjectInputStream. (ByteArrayInputStream. bs))))

(defn round-trip [x]
  (bytes->obj (obj->bytes x)))

(defn equiv? [lhs rhs]
  (cond
    (and (instance? JSValue lhs)
         (instance? JSValue rhs))
    (equiv? (.-val lhs) (.-val rhs))

    (and (vector? lhs)
         (vector? rhs))
    (and (= (count lhs) (count rhs))
         (every? #(apply equiv? %) (map vector lhs rhs)))

    (and (map? lhs)
         (map? rhs))
    (and (= (count lhs) (count rhs))
         (every? (fn [k]
                   (equiv? (get lhs k) (get rhs k)))
           (keys lhs)))

    :else
    (= lhs rhs)))

(deftest test-JSValue-serialization
  (are [x] (equiv? x (.-val (round-trip (tags/->JSValue x))))
           []
           [1 2 3]
           [1 2 (tags/->JSValue [3 4])]
           [1 2 (tags/->JSValue [3 (tags/->JSValue {:a 1})])]
           [1 2 (tags/->JSValue {:a 1})]
           [1 2 (tags/->JSValue {:a (tags/->JSValue [3 4])})]
           {}
           {:a 1}
           {"n/a" 2}
           {:x (tags/->JSValue [3 4])}
           {:x (tags/->JSValue [3 (tags/->JSValue {:a 1})])}
           {:x (tags/->JSValue {:a 1})}
           {:x (tags/->JSValue {:a (tags/->JSValue [3 4])})}))
