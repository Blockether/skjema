(ns com.blockether.skjema.hostile-test
  "Schemas and instances written to break the evaluator rather than to be
   validated: recursion deep enough to exhaust a stack, a regular expression
   that does not parse, a reference whose escape is malformed, a number JSON
   has no spelling for. Each of these used to hand the caller something it
   could not catch - a raw PatternSyntaxException, a NumberFormatException at
   validation time, a killed thread - and each one is answered here."
  (:require [com.blockether.skjema.core :as skjema]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(def ^:private recursive-list
  "A schema that refers to itself. Nothing in the reference chain shrinks, so
   only the instance ends the descent."
  {"$defs" {"node" {"anyOf" [{"type" "null"}
                             {"type" "array" "items" {"$ref" "#/$defs/node"}}]}}
   "$ref" "#/$defs/node"})

(defn- nested-array [depth]
  (loop [i 0 value nil]
    (if (= i depth) value (recur (inc i) [value]))))

(defn- nested-schema [depth]
  (loop [i 0 schema {"type" "object"}]
    (if (= i depth) schema (recur (inc i) {"type" "object" "properties" {"a" schema}}))))

(defn- refusal
  "The exception `f` refuses with, or nil where it answered."
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e e)))

(deftest an-instance-deeper-than-the-stack-is-refused-and-not-fatal
  (let [compiled (skjema/compile-schema recursive-list)]
    (is (true? (skjema/validate compiled (nested-array 100))))
    (testing "past the bound both answers are an error the caller can catch"
      (doseq [answer [#(skjema/validate compiled (nested-array 400))
                      #(skjema/explain compiled (nested-array 400))]]
        (is (= :schema/recursion (:skjema/error (ex-data (refusal answer)))))))))

(deftest a-schema-nested-deeper-than-the-stack-is-refused-at-compilation
  (is (= :schema/recursion
         (:skjema/error (ex-data (refusal #(skjema/compile-schema (nested-schema 400))))))))

(deftest an-unparseable-pattern-names-the-keyword-that-holds-it
  (doseq [[schema location] [[{"pattern" "("} "/pattern"]
                             [{"properties" {"a" {"pattern" "["}}} "/properties/a/pattern"]
                             [{"patternProperties" {"(" {}}} "/patternProperties/("]]]
    (let [error (refusal #(skjema/compile-schema schema))]
      (is (= :schema/invalid (:skjema/error (ex-data error))))
      (is (= location (:keywordLocation (ex-data error))))
      (is (instance? java.util.regex.PatternSyntaxException (ex-cause error))))))

(deftest a-malformed-percent-escape-resolves-to-nothing-rather-than-throwing
  (is (= :schema/unresolved-ref
         (:skjema/error (ex-data (refusal #(skjema/validate {"$ref" "#/%zz"} 1))))))
  (testing "a complete escape still names the member it spells"
    (is (true? (skjema/validate {"$defs" {"a b" {"type" "integer"}}
                                 "$ref" "#/$defs/a%20b"}
                                1)))))

(deftest a-ratio-is-compared-by-every-path-instead-of-refused
  (doseq [[schema instance] [[{"multipleOf" 0.5} 1/3]
                             [{"maximum" 0} 1/3]
                             [{"exclusiveMinimum" 0} 1/3]
                             [{"enum" [[1/3]]} [1/3]]]]
    (let [compiled (skjema/compile-schema schema)
          complete (assoc compiled :fast-validator nil :fast-explainer nil)]
      (is (= (skjema/validate complete instance) (skjema/validate compiled instance)))
      (is (= (skjema/explain complete instance) (skjema/explain compiled instance))))))

(deftest a-number-json-cannot-spell-compiles-and-explains
  (doseq [value [##NaN ##Inf ##-Inf]]
    (let [compiled (skjema/compile-schema {"properties" {"a" {"const" value}}})
          [error] (:errors (skjema/explain compiled {"a" 1}))]
      (is (false? (skjema/validate compiled {"a" 1})))
      (is (= "const" (:keyword error))))))

(deftest a-number-json-cannot-spell-is-one-value-and-not-a-string
  (doseq [[schema instance verdict] [[{"uniqueItems" true} [##NaN -0.0 ##NaN] false]
                                     [{"uniqueItems" true} [##NaN ##Inf] true]
                                     [{"uniqueItems" true} [##NaN "NaN"] true]
                                     [{"const" ##NaN} ##NaN true]]]
    (let [compiled (skjema/compile-schema schema)
          complete (assoc compiled :fast-validator nil :fast-explainer nil)]
      (is (= verdict (skjema/validate compiled instance)))
      (is (= verdict (skjema/validate complete instance))))))
