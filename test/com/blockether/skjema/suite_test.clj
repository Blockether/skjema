(ns com.blockether.skjema.suite-test
  "Conformance for the validator, measured by json-schema-org/JSON-Schema-Test-Suite.

   Every file under `tests/draft2020-12` is a list of groups; a group carries
   one schema and the instances it must accept or reject. `optional/` is in
   the gate as well, because everything it asks for is implemented: the
   formats, ECMAScript regular expressions, arbitrary-precision numbers, the
   draft-07 `dependencies` and a reference into a historic draft. The one
   thing those files need that the required ones must NOT have is `format`
   asserting instead of annotating - the specification leaves that to the
   caller - so the format directory is the only one run with
   `:format-assertion true`.

   Remote references resolve from the suite's own `remotes/` directory, handed
   in as a registry. Nothing is fetched: a validator that reaches the network
   fails differently on every machine."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.skjema.core :as skjema])
  (:import (java.io File)))

(def ^:private suite-root "test/resources/JSON-Schema-Test-Suite")

(def ^:private remotes
  (delay
    (let [root (io/file suite-root "remotes")
          prefix (inc (count (.getPath root)))]
      (into {}
            (for [^File f (file-seq root)
                  :when (and (.isFile f) (str/ends-with? (.getName f) ".json"))]
              [(str "http://localhost:1234/" (subs (.getPath f) prefix))
               (skjema/read-schema f)])))))

(defn- json-files [^File dir recursive?]
  (->> (if recursive? (file-seq dir) (.listFiles dir))
       (filter (fn [^File f] (and (.isFile f) (str/ends-with? (.getName f) ".json"))))
       (sort-by (fn [^File f] (.getPath f)))))

(defn- run-file
  "Every disagreement between the suite and the validator, as data."
  [^File f opts]
  (for [group (skjema/read-schema f)
        :let [compiled (try
                         (skjema/compile-schema (get group "schema") (merge {:registry @remotes} opts))
                         (catch Throwable t t))]
        test-case (get group "tests")
        :let [expected (get test-case "valid")
              actual (try
                       (if (instance? Throwable compiled)
                         (throw ^Throwable compiled)
                         (skjema/valid? compiled (get test-case "data")))
                       (catch Throwable e (str "threw: " (.getMessage e))))]
        :when (not= expected actual)]
    {:file (.getName f)
     :group (get group "description")
     :test (get test-case "description")
     :expected expected
     :actual actual}))

(defn- assertion-count [files]
  (reduce + (for [^File f files, group (skjema/read-schema f)]
              (count (get group "tests")))))

(deftest json-schema-test-suite
  (let [root (io/file suite-root "tests/draft2020-12")
        required (json-files root false)
        optional (json-files (io/file root "optional") true)
        format? (fn [^File f] (str/includes? (.getPath f) "/optional/format/"))
        failures (vec (concat
                       (mapcat #(run-file % nil) required)
                       (mapcat #(run-file % (when (format? %) {:format-assertion true})) optional)))]
    (testing "the vendored suite is present, required files and optional ones alike"
      (is (<= 40 (count required)))
      (is (<= 30 (count optional)))
      (is (<= 2300 (+ (assertion-count required) (assertion-count optional)))))
    (when (seq failures)
      (println "\n" (count failures) "suite failures")
      (doseq [[file fs] (sort-by key (group-by :file failures))]
        (println (format "%-32s %d" file (count fs)))
        (doseq [x (take 3 fs)]
          (println "   -" (:group x) "|" (:test x) "| expected" (:expected x) "got" (:actual x)))))
    (is (empty? failures) (str (count failures) " assertions of the suite disagree"))))

(deftest compiled-predicates-match-the-complete-evaluator
  (let [files (json-files (io/file suite-root "tests/draft2020-12") false)
        checked (atom 0)
        disagreements
        (vec
         (for [^File file files
               group (skjema/read-schema file)
               :let [compiled (skjema/compile-schema (get group "schema"))]
               :when (:fast-validator compiled)
               test-case (get group "tests")
               :let [instance (get test-case "data")
                     expected (get test-case "valid")
                     fast (skjema/valid? compiled instance)
                     complete (skjema/valid? (assoc compiled :fast-validator nil) instance)
                     _ (swap! checked inc)]
               :when (not= expected fast complete)]
           {:file (.getName file)
            :group (get group "description")
            :test (get test-case "description")
            :expected expected
            :fast fast
            :complete complete}))]
    (is (< 100 @checked) "the official suite did not exercise the compiled predicate")
    (is (empty? disagreements) (pr-str (take 5 disagreements)))))
