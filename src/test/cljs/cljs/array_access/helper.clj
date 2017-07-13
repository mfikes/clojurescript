(ns cljs.array-access.helper
  (:require [cljs.analyzer :as ana]))

(defmacro unchecked-arrays? []
  (ana/unchecked-arrays?))

(defmacro squelching-console-warn
  [& body]
  `(let [existing-console-warn# (and (cljs.core/exists? js/console)
                                     js/console.warn)]
     (when existing-console-warn#
       (set! js/console.warn (fn [])))
     ~@body
     (when existing-console-warn#
       (set! js/console.warn existing-console-warn#))))
