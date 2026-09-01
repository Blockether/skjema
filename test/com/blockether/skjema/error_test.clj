(ns com.blockether.skjema.error-test
  "Structured validation errors, adapted from Ajv's public error-contract suite.

   Skjema keeps JSON Schema's standard BASIC-output locations and adds the two
   machine-readable fields callers otherwise have to scrape out of prose:
   `keyword` and keyword-specific `params`."
  (:require [com.blockether.skjema.core :as skjema]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest a-nested-error-is-both-standard-and-machine-readable
  (let [schema {"$id" "https://example.com/root.json"
                "$defs" {"number" {"type" "number"}}
                "properties" {"foo" {"$ref" "#/$defs/number"}}}
        [error] (:errors (skjema/validate schema {"foo" "not-a-number"}))]
    (is (= {:instanceLocation "/foo"
            :keywordLocation "/properties/foo/$ref/type"
            :absoluteKeywordLocation "https://example.com/root.json#/$defs/number/type"
            :keyword "type"
            :params {:type "number"}
            :error "expected number, got string"}
           error))))

(deftest additional-properties-name-each-property-without-inventing-a-child-location
  ;; Adapted from Ajv errors.spec.ts: the property does not exist according to
  ;; the schema, so its name belongs in params and the instance path stays at
  ;; the object that owns it.
  (let [schema {"properties" {"foo" {} "bar" {}}
                "additionalProperties" false}
        errors (:errors (skjema/validate schema {"foo" 1 "bar" 2 "baz" 3 "quux" 4}))]
    (is (= [{:instanceLocation ""
             :keywordLocation "/additionalProperties"
             :keyword "additionalProperties"
             :params {:additionalProperty "baz"}
             :error "additional property \"baz\" is not allowed"}
            {:instanceLocation ""
             :keywordLocation "/additionalProperties"
             :keyword "additionalProperties"
             :params {:additionalProperty "quux"}
             :error "additional property \"quux\" is not allowed"}]
           errors))))

(deftest required-names-one-missing-property-per-error
  ;; Adapted from Ajv errors.spec.ts: every missing member is independently
  ;; actionable, rather than hidden inside one sentence callers must parse.
  (let [errors (:errors (skjema/validate {"required" ["foo" "bar" "baz"]} {"foo" 1}))]
    (is (= [{:instanceLocation ""
             :keywordLocation "/required"
             :keyword "required"
             :params {:missingProperty "bar"}
             :error "missing required property \"bar\""}
            {:instanceLocation ""
             :keywordLocation "/required"
             :keyword "required"
             :params {:missingProperty "baz"}
             :error "missing required property \"baz\""}]
           errors))))

(deftest one-of-reports-the-branches-that-passed
  ;; Adapted from Ajv errors.spec.ts.
  (let [schema {"oneOf" [{"type" "number"} {"type" "integer"} {"const" 1.5}]}
        [integer-error] (:errors (skjema/validate schema 1))
        [decimal-error] (:errors (skjema/validate schema 1.5))]
    (is (= {:passingSchemas [0 1]} (:params integer-error)))
    (is (= {:passingSchemas [0 2]} (:params decimal-error)))
    (is (= "oneOf" (:keyword integer-error)))))

(deftest limits-and-duplicates-carry-values-a-ui-can-render
  (testing "numeric and size limits"
    (let [errors (:errors (skjema/validate {"maxLength" 3} "long"))]
      (is (= {:limit 3 :actual 4} (:params (first errors))))))
  (testing "duplicate array positions"
    (let [[error] (:errors (skjema/validate {"uniqueItems" true} ["a" "b" "a"]))]
      (is (= "uniqueItems" (:keyword error)))
      (is (= {:i 2 :j 0} (:params error))))))

(deftest dependencies-name-each-trigger-and-missing-property
  ;; Adapted from Ajv errors.spec.ts.
  (let [schema {"$schema" "http://json-schema.org/draft-07/schema#"
                "dependencies" {"a" ["foo" "bar"]}}
        errors (:errors (skjema/validate schema {"a" 0}))]
    (is (= [{:property "a"
             :missingProperty "foo"
             :deps "foo, bar"
             :depsCount 2}
            {:property "a"
             :missingProperty "bar"
             :deps "foo, bar"
             :depsCount 2}]
           (mapv :params errors)))
    (is (= ["dependencies" "dependencies"] (mapv :keyword errors)))))

(deftest structured-errors-remain-json-values
  (let [answer (-> (skjema/validate {"required" ["name"]} {})
                   skjema/write-schema
                   skjema/read-schema)]
    (is (= {"valid" false
            "errors" [{"instanceLocation" ""
                       "keywordLocation" "/required"
                       "keyword" "required"
                       "params" {"missingProperty" "name"}
                       "error" "missing required property \"name\""}]}
           answer))))
