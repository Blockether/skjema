(ns com.blockether.skjema.validate-test
  "What a caller reads back: the specification's BASIC output, the options
   that change what asserts, and the keywords a schema older than 2020-12
   still means."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.skjema.core :as skjema]))

(def ^:private user-schema
  {"$id" "https://example.com/user.json"
   "type" "object"
   "properties" {"name" {"type" "string" "minLength" 3}
                 "tags" {"type" "array" "items" {"$ref" "#/$defs/tag"}}}
   "required" ["name"]
   "$defs" {"tag" {"type" "string"}}})

(deftest a-valid-instance-answers-nothing-else
  (is (= {:valid true} (skjema/validate user-schema {"name" "ada" "tags" ["one"]})))
  (is (true? (skjema/valid? user-schema {"name" "ada"}))))

(deftest every-error-says-where-it-happened
  (let [{:keys [valid errors]} (skjema/validate user-schema {"name" "ab" "tags" ["ok" 7]})]
    (is (false? valid))
    (is (= 2 (count errors)))
    (testing "the instance location is a JSON pointer into the instance"
      (is (= ["/name" "/tags/1"] (mapv :instanceLocation errors))))
    (testing "the keyword location is the path taken through the schema, the reference included"
      (is (= ["/properties/name/minLength" "/properties/tags/items/$ref/type"]
             (mapv :keywordLocation errors))))
    (testing "the absolute location resolves that path against the identifier of the resource it ended in"
      (is (= ["https://example.com/user.json#/properties/name/minLength"
              "https://example.com/user.json#/$defs/tag/type"]
             (mapv :absoluteKeywordLocation errors))))
    (testing "and the message names what was actually wrong"
      (is (= ["the string is 2 characters long, the minimum is 3"
              "expected string, got integer"]
             (mapv :error errors))))))

(deftest the-answer-is-the-specifications-own-json
  (is (= {"valid" false
          "errors" [{"instanceLocation" ""
                     "keywordLocation" "/required"
                     "keyword" "required"
                     "params" {"missingProperty" "name"}
                     "absoluteKeywordLocation" "https://example.com/user.json#/required"
                     "error" "missing required property \"name\""}]}
         (skjema/read-schema (skjema/write-schema (skjema/validate user-schema {}))))))

(deftest a-schema-without-an-identifier-has-no-absolute-location
  (let [[error] (:errors (skjema/validate {"type" "integer"} "no"))]
    (is (= "" (:instanceLocation error)))
    (is (= "/type" (:keywordLocation error)))
    (is (not (contains? error :absoluteKeywordLocation)))))

(deftest format-annotates-until-it-is-asked-to-assert
  (let [schema {"type" "string" "format" "ipv4"}]
    (is (true? (skjema/valid? schema "not-an-address")))
    (is (false? (skjema/valid? schema "not-an-address" {:format-assertion true})))
    (is (true? (skjema/valid? schema "127.0.0.1" {:format-assertion true})))
    (testing "a format this library does not know asserts nothing at all"
      (is (true? (skjema/valid? {"format" "sort-code"} "anything" {:format-assertion true}))))
    (testing "and the failure reads like every other error"
      (is (= [{:instanceLocation "" :keywordLocation "/format"
               :keyword "format" :params {:format "ipv4"}
               :error "the string is not a valid ipv4"}]
             (:errors (skjema/validate schema "not-an-address" {:format-assertion true})))))))

(deftest the-format-assertion-vocabulary-asks-on-the-schemas-behalf
  (let [meta-schema {"$id" "https://example.com/format-meta"
                     "$schema" "https://json-schema.org/draft/2020-12/schema"
                     "$vocabulary" {"https://json-schema.org/draft/2020-12/vocab/core" true
                                    "https://json-schema.org/draft/2020-12/vocab/format-assertion" false}}
        schema {"$id" "https://example.com/asserting"
                "$schema" "https://example.com/format-meta"
                "format" "ipv4"}
        compiled (skjema/compile-schema schema {:registry {"https://example.com/format-meta" meta-schema}})]
    (testing "declaring the vocabulary is the request; the boolean beside it only speaks to implementations without it"
      (is (false? (skjema/valid? compiled "not-an-address")))
      (is (true? (skjema/valid? compiled "127.0.0.1"))))))

(deftest dependencies-still-mean-what-they-meant
  (let [schema {"dependencies" {"card" ["billing"]
                                "billing" {"required" ["postcode"]}}}]
    (is (true? (skjema/valid? schema {})))
    (is (false? (skjema/valid? schema {"card" 1})))
    (is (false? (skjema/valid? schema {"billing" 1})))
    (is (true? (skjema/valid? schema {"card" 1 "billing" 1 "postcode" "SW1"})))))

(deftest a-resource-is-read-as-the-draft-it-declares
  (let [older {"$id" "https://example.com/2019.json"
               "$schema" "https://json-schema.org/draft/2019-09/schema"
               "prefixItems" [{"type" "string"}]}
        schema {"$schema" "https://json-schema.org/draft/2020-12/schema"
                "type" "array"
                "$ref" "https://example.com/2019.json"}]
    (testing "2019-09 has no `prefixItems`, so a reference into one finds an annotation"
      (is (true? (skjema/valid? schema [1 2 3]
                                {:registry {"https://example.com/2019.json" older}}))))
    (testing "while the same schema in 2020-12 asserts"
      (is (false? (skjema/valid?
                   (assoc older "$schema" "https://json-schema.org/draft/2020-12/schema")
                   [1 2 3]))))))

(deftest draft-07-spells-a-tuple-with-items
  (let [schema {"$schema" "http://json-schema.org/draft-07/schema#"
                "items" [{"type" "string"} {"type" "number"}]
                "additionalItems" {"type" "boolean"}}]
    (is (true? (skjema/valid? schema ["a" 1 true])))
    (is (false? (skjema/valid? schema [1])))
    (is (false? (skjema/valid? schema ["a" 1 "and one too many"])))))

(deftest malformed-schemas-fail-at-compilation-with-actionable-data
  (doseq [[schema location] [[{"type" "mystery"} "/type"]
                             [{"required" "name"} "/required"]
                             [{"multipleOf" 0} "/multipleOf"]
                             [{"minLength" -1} "/minLength"]]]
    (let [error (try (skjema/compile-schema schema) nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (= :schema/invalid (:skjema/error (ex-data error))))
      (is (= location (:instanceLocation (first (:errors (ex-data error))))))
      (is (re-find (re-pattern (java.util.regex.Pattern/quote location)) (ex-message error)))))
  (let [error (try (skjema/compile-schema {"patternProperties" {"[" true}}) nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :schema/invalid (:skjema/error (ex-data error))))
    (is (= "/patternProperties/[" (:keywordLocation (ex-data error))))
    (is (instance? java.util.regex.PatternSyntaxException (ex-cause error)))))

(deftest compiled-numeric-path-is-total-and-matches-complete-evaluation
  (doseq [[schema instances]
          [[{"type" "integer"} [(float 1) (short 1) (byte 1) (float 1.5)]]
           [{"minimum" 0} [1/2 -1/2 ##NaN ##Inf]]
           [{"enum" [1/2]} [1/2 0.5 1]]]]
    (let [compiled (skjema/compile-schema schema)
          complete (assoc compiled :fast-validator nil)]
      (doseq [instance instances]
        (is (= (skjema/valid? complete instance)
               (skjema/valid? compiled instance)))))))
