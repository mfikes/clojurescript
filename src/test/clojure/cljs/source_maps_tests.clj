(ns cljs.source-maps-tests
  (:use clojure.test)
  (:require [cljs.build.api :as build]
            [cljs.test-util :as test]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.util.regex Pattern)))

; -- helpers ----------------------------------------------------------------------------------------------------------------

(deftype NegativeCheck [query]
  Object
  (toString [_this]
    (str "negative check for '" (str query) "' (" (type query) ")")))

(defn ! [query]
  (NegativeCheck. query))

(defn check-file [path query]
  (let [file (io/file path)
        exists? (.exists file)]
    (cond
      (false? query) (not exists?)
      (true? query) exists?
      :else (if exists?
              (let [content (slurp (io/file file))]
                (cond
                  (nil? query) true
                  (string? query) (string/includes? content query)
                  (fn? query) (query content)
                  (instance? Pattern query) (some? (re-find query content))
                  (sequential? query) (every? #(check-file path %) query)
                  (instance? NegativeCheck query) (not (check-file path (.-query query)))
                  :else (throw (ex-info "Invalid matching query" {:query query}))))))))

(defn project-with-source-maps [output-dir & [extra-opts]]
  (let [base-opts {:main          "source-maps.core"
                   :output-dir    output-dir
                   :optimizations :none}]
    {:inputs (str (io/file "src" "test" "cljs_build" "source_maps"))
     :opts   (merge base-opts extra-opts)}))

(defn build-project-with-source-maps [build-name & [extra-opts]]
  (println (str "Compiling source-maps test '" build-name "' ..."))
  (let [out (str (io/file (test/tmp-dir) "cljs-tests-source-maps-build" build-name))
        project (project-with-source-maps out extra-opts)]
    (test/delete-out-files out)
    (build/build (build/inputs (:inputs project)) (:opts project))
    out))

(defn build-result [out & args]
  (str (apply io/file out "source_maps" args)))

; -- tests ------------------------------------------------------------------------------------------------------------------

(deftest test-external-source-maps
  (testing "source maps with defaults under :none optimizations"
    (let [out (build-project-with-source-maps "source-maps-onone")]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.cljs" true
        "main.js" ["sourceMappingURL=main.js.map" (! "rel=")]
        "main.js.map" ["\"version\":3" "\"sources\":[\"main.cljs\"]"]
        "utils.cljs" true
        "utils.js" ["sourceMappingURL=utils.js.map" (! "rel=")]
        "utils.js.map" ["\"version\":3" "\"sources\":[\"utils.cljs\"]"])))
  (testing "disable source maps under :none optimizations"
    (let [opts {:source-map false}
          out (build-project-with-source-maps "source-maps-onone-disabled" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.cljs" false
        "main.js" (! "sourceMappingURL")
        "main.js.map" false
        "utils.cljs" false
        "utils.js" (! "sourceMappingURL")
        "utils.js.map" false)))
  (testing "source maps with :source-map-asset-path under :none optimizations"
    (let [opts {:source-map-asset-path "http://localhost:1234/some/path"}
          out (build-project-with-source-maps "source-maps-onone-source-map-asset-path" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.cljs" true
        "main.js" ["sourceMappingURL=http://localhost:1234/some/path/source_maps/main.js.map" (! "rel=")]
        "main.js.map" ["\"version\":3" "\"sources\":[\"main.cljs\"]"]
        "utils.cljs" true
        "utils.js" ["sourceMappingURL=http://localhost:1234/some/path/source_maps/utils.js.map" (! "rel=")]
        "utils.js.map" ["\"version\":3" "\"sources\":[\"utils.cljs\"]"])))
  (testing "source maps with :source-map-url under :none optimizations"
    (let [opts {:source-map-url "hard/coded/path.js.map"}
          out (build-project-with-source-maps "source-maps-onone-source-map-url" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.cljs" true
        "main.js" ["sourceMappingURL=hard/coded/path.js.map" (! "rel=")]
        "main.js.map" ["\"version\":3" "\"sources\":[\"main.cljs\"]"]
        "utils.cljs" true
        "utils.js" ["sourceMappingURL=hard/coded/path.js.map" (! "rel=")]
        "utils.js.map" ["\"version\":3" "\"sources\":[\"utils.cljs\"]"])))
  (testing "source maps with defaults and timestamps under :none optimizations"
    (let [opts {:source-map-timestamp true}
          out (build-project-with-source-maps "source-maps-onone-timestamps" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.js" #"sourceMappingURL=main\.js\.map\?rel=\d+"
        "utils.js" #"sourceMappingURL=utils\.js\.map\?rel=\d+")))
  (testing "source maps with :source-map-asset-path and timestamps under :none optimizations"
    (let [opts {:source-map-timestamp  true
                :source-map-asset-path "http://localhost:1234/some/path"}
          out (build-project-with-source-maps "source-maps-onone-source-map-asset-path-timestamps" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.js" #"sourceMappingURL=http://localhost:1234/some/path/source_maps/main\.js\.map\?rel=\d+"
        "utils.js" #"sourceMappingURL=http://localhost:1234/some/path/source_maps/utils\.js\.map\?rel=\d+")))
  (testing "source maps with :source-map-url and timestamps under :none optimizations"
    (let [opts {:source-map-timestamp true
                :source-map-url       "hard/coded/path.js.map?some=param&another=param2"}
          out (build-project-with-source-maps "source-maps-onone-source-map-url-timestamps" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.js" #"sourceMappingURL=hard/coded/path\.js\.map\?some=param&another=param2&rel=\d+"
        "utils.js" #"sourceMappingURL=hard/coded/path\.js\.map\?some=param&another=param2&rel=\d+"))))

(deftest test-inlined-source-maps
  (testing "inlined source maps with defaults under :none optimizations"
    (let [opts {:inline-source-maps true}
          out (build-project-with-source-maps "source-maps-inlined-onone" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.cljs" true
        "main.js" "sourceMappingURL=data:application/json;base64,"
        "main.js.map" false
        "utils.cljs" true
        "utils.js" "sourceMappingURL=data:application/json;base64,"
        "utils.js.map" false)))
  (testing ":inline-source-maps should have no effect with disabled source maps under :none optimizations"
    (let [opts {:source-map         false
                :inline-source-maps true}
          out (build-project-with-source-maps "source-maps-inlined-onone-disabled" opts)]
      (are [file pattern] (check-file (build-result out file) pattern)
        "main.cljs" false
        "main.js" (! "sourceMappingURL")
        "main.js.map" false
        "utils.cljs" false
        "utils.js" (! "sourceMappingURL")
        "utils.js.map" false))))
