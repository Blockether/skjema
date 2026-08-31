(ns com.blockether.skjema.json-test
  "Conformance for the reader, measured by nst/JSONTestSuite.

   The suite names the verdict in the file name: `y_` MUST parse, `n_` MUST be
   rejected, `i_` is implementation-defined and may go either way as long as the
   reader answers at all. Only an `ExceptionInfo` counts as a rejection here -
   any other throwable means the reader broke instead of refusing, and a
   StackOverflowError deliberately escapes so a missing depth bound cannot be
   mistaken for a well-formed refusal."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing thrown?]]
            [com.blockether.skjema.json :as json])
  (:import (java.io File)
           (java.nio.file Files)))

(defn- suite-files []
  (->> (.listFiles (io/file "test/resources/JSONTestSuite/test_parsing"))
       (filter (fn [^File f] (and (.isFile f) (str/ends-with? (.getName f) ".json"))))
       (sort-by (fn [^File f] (.getName f)))))

(defn- outcome [^File f]
  (let [bytes (Files/readAllBytes (.toPath f))]
    (try
      (json/read-bytes bytes)
      :accepted
      (catch clojure.lang.ExceptionInfo _ :rejected))))

(deftest json-test-suite-parsing
  (let [files (suite-files)
        kind (fn [^File f] (subs (.getName f) 0 2))
        by-kind (group-by kind files)
        names (fn [fs] (mapv (fn [^File f] (.getName f)) fs))]
    (testing "the vendored suite is present"
      (is (<= 300 (count files)) "test/resources/JSONTestSuite/test_parsing is missing"))
    (testing "y_ - every well-formed document parses"
      (let [wrong (names (remove #(= :accepted (outcome %)) (get by-kind "y_")))]
        (is (empty? wrong) (str "rejected valid JSON: " (str/join ", " wrong)))))
    (testing "n_ - every malformed document is refused"
      (let [wrong (names (remove #(= :rejected (outcome %)) (get by-kind "n_")))]
        (is (empty? wrong) (str "accepted invalid JSON: " (str/join ", " wrong)))))
    (testing "i_ - implementation-defined input still answers"
      (is (every? #{:accepted :rejected} (map outcome (get by-kind "i_")))))))

(deftest values
  (testing "objects keep STRING keys and arrays become vectors"
    (is (= {"a" 1 "b" [true false nil]} (json/read-str "{\"a\":1,\"b\":[true,false,null]}")))
    (is (vector? (json/read-str "[1,2]"))))
  (testing "any value may stand alone at the top level"
    (is (= 1 (json/read-str "1")))
    (is (= "x" (json/read-str "\"x\"")))
    (is (nil? (json/read-str "null"))))
  (testing "integers are Long until they no longer fit"
    (is (instance? Long (json/read-str "42")))
    (is (= 42 (json/read-str "42")))
    (is (instance? BigInteger (json/read-str "123456789012345678901234567890"))))
  (testing "a fraction or an exponent makes it a Double - 1.0 is still an integer VALUE"
    (is (= 1.0 (json/read-str "1.0")))
    (is (instance? Double (json/read-str "1e2")))
    (is (= 100.0 (json/read-str "1e2"))))
  (testing "escapes, including a surrogate pair"
    (is (= "\"\\/\b\f\n\r\t" (json/read-str "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"")))
    (is (= "\uD83D\uDE00" (json/read-str "\"\\uD83D\\uDE00\"")))
    (is (= 2 (count (json/read-str "\"\\uD83D\\uDE00\"")))))
  (testing "the last duplicate key wins"
    (is (= {"a" 2} (json/read-str "{\"a\":1,\"a\":2}")))))

(deftest refusals
  (let [rejected? (fn [s] (try (json/read-str s) false
                               (catch clojure.lang.ExceptionInfo e
                                 (= :json/parse (:skjema/error (ex-data e))))))]
    (testing "the grammar is the whole grammar"
      (is (rejected? "01"))
      (is (rejected? ".5"))
      (is (rejected? "5."))
      (is (rejected? "+1"))
      (is (rejected? "NaN"))
      (is (rejected? "Infinity"))
      (is (rejected? "[1,]"))
      (is (rejected? "{\"a\":1,}"))
      (is (rejected? "{a:1}"))
      (is (rejected? "'x'"))
      (is (rejected? ""))
      (is (rejected? "\uFEFF{}")))
    (testing "one document, not a stream"
      (is (rejected? "{} {}"))
      (is (rejected? "1 2")))
    (testing "a control character inside a string must be escaped"
      (is (rejected? "\"a\nb\"")))
    (testing "an error carries where it happened"
      (let [data (try (json/read-str "{\n  \"a\": }") nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :json/parse (:skjema/error data)))
        (is (= 2 (:line data)))
        (is (pos? (:column data)))))
    (testing "depth is bounded, so hostile nesting is a refusal and not an Error"
      (is (rejected? (str (str/join (repeat 5000 "[")) (str/join (repeat 5000 "]")))))
      (is (= (nth (iterate vector 1) 500)
             (json/read-str (str (str/join (repeat 500 "[")) "1" (str/join (repeat 500 "]")))))))
    (testing "bytes that are not UTF-8 are refused before parsing"
      (let [bad (byte-array [(byte 0x22) (byte -1) (byte 0x22)])]
        (is (= :json/encoding
               (try (json/read-bytes bad) nil
                    (catch clojure.lang.ExceptionInfo e (:skjema/error (ex-data e))))))))))

(deftest writing
  (testing "round trip through both directions"
    (let [text "{\"a\":[1,2.5,true,null,\"x\"],\"b\":{}}"]
      (is (= text (json/write-str (json/read-str text))))))
  (testing "keywords are written as their name, on both sides of a pair"
    (is (= "{\"a\":\"b\"}" (json/write-str {:a :b}))))
  (testing "control characters and quotes are escaped"
    (is (= "\"a\\nb\\\"c\\u0001\"" (json/write-str "a\nb\"c\u0001"))))
  (testing "what JSON cannot express is refused, never coerced"
    (is (thrown? clojure.lang.ExceptionInfo (json/write-str ##NaN)))
    (is (thrown? clojure.lang.ExceptionInfo (json/write-str ##Inf)))
    (is (thrown? clojure.lang.ExceptionInfo (json/write-str {1 2})))
    (is (thrown? clojure.lang.ExceptionInfo (json/write-str (java.util.Date.))))))
