(ns com.blockether.skjema.io-test
  (:require [clojure.java.io :as io]
            [com.blockether.skjema.core :as skjema]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing thrown?]])
  (:import (java.nio.file Files Path)
           (java.nio.file.attribute FileAttribute)))

(deftest schemas-come-from-text-path-file-or-resource
  (let [text "{\"type\":\"string\"}"
        ^Path path (Files/createTempFile "skjema-" ".json" (make-array FileAttribute 0))]
    (try
      (spit (.toFile path) text)
      (is (= {"type" "string"} (skjema/read-schema text)))
      (is (= {"type" "string"} (skjema/read-schema path)))
      (is (= {"type" "string"} (skjema/read-schema (.toFile path))))
      (is (= "https://json-schema.org/draft/2020-12/schema"
             (get (skjema/read-schema
                   (io/resource "com/blockether/skjema/meta/2020-12/schema.json"))
                  "$id")))
      (finally
        (Files/deleteIfExists path)))))

(deftest schema-io-failures-have-one-public-shape
  (testing "a parser failure names the operation without copying input into error data"
    (let [error (try (skjema/read-schema "{\"type\":}") nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (= :schema/read (:skjema/error (ex-data error))))
      (is (= "JSON string" (:source (ex-data error))))
      (is (instance? charred.CharredException (ex-cause error)))
      (is (re-find #"could not read schema from JSON string" (ex-message error)))))
  (is (= :schema/read
         (try (skjema/read-schema nil) nil
              (catch clojure.lang.ExceptionInfo e (:skjema/error (ex-data e))))))
  (is (= :schema/write
         (try (skjema/write-schema ##NaN) nil
              (catch clojure.lang.ExceptionInfo e (:skjema/error (ex-data e)))))))

(deftest writing-round-trips-json-values
  (let [value {:valid false :errors [{:keyword "type" :params {:type "string"}}]}
        text (skjema/write-schema value)]
    (is (= {"valid" false
            "errors" [{"keyword" "type" "params" {"type" "string"}}]}
           (skjema/read-schema text)))
    (is (not (re-find #"\\/" (skjema/write-schema {"$id" "https://example.com/schema"}))))
    (is (thrown? clojure.lang.ExceptionInfo (skjema/write-schema {1 2})))))
