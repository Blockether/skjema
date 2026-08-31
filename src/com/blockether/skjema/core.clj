(ns com.blockether.skjema.core
  "JSON Schema 2020-12 evaluation.

   The evaluator answers ANNOTATIONS as well as errors, from the first line and
   not as a later addition: `unevaluatedProperties` and `unevaluatedItems` are
   defined in terms of what the ADJACENT keywords and the in-place applicators
   already evaluated, so a validator that answers only true/false cannot grow
   them without being rewritten. Every evaluation therefore returns
   `{:valid? :props :items :errors}` - the property names and item indices that
   were evaluated AT THIS INSTANCE LOCATION, plus every fault found underneath.

   Two kinds of applicator, and the difference is the whole design:

   - IN PLACE (`$ref`, `$dynamicRef`, `allOf`, `anyOf`, `oneOf`, `if`/`then`/
     `else`, `dependentSchemas`) apply another schema to the SAME instance
     location, so their annotations belong to this location and bubble up.
   - CHILD (`properties`, `patternProperties`, `additionalProperties`, `items`,
     `prefixItems`, `contains`, `propertyNames`) apply to a location BELOW, so
     their annotations stay there and this location records only which member
     was covered.

   Identifiers are resolved once, when the schema is compiled: `$id` moves the
   base URI, `$anchor` and `$dynamicAnchor` name a place inside it, and the
   index that comes out is what `$ref` and `$dynamicRef` read. Nothing is
   fetched over the network, ever - a schema that references a document the
   caller did not supply is a compile error, not a silent pass."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.skjema.format :as fmt]
            [com.blockether.skjema.json :as json]
            [com.blockether.skjema.regex :as regex]
            [com.blockether.skjema.uri :as uri])
  (:import (java.math BigDecimal)))

(set! *warn-on-reflection* true)

(def ^:private max-eval-depth
  "A cyclic schema (`{\"$ref\": \"#\"}`) recurses without the instance ever
   shrinking. Data-driven recursion terminates on its own; this bound only
   turns the pathological schema into an error a caller can catch."
  2048)

(def ^:private validation-vocabulary
  "https://json-schema.org/draft/2020-12/vocab/validation")

(def ^:private format-assertion-vocabulary
  "https://json-schema.org/draft/2020-12/vocab/format-assertion")

(def ^:private official-meta-schema
  "https://json-schema.org/draft/2020-12/schema")

(def ^:private bundled-meta-schemas
  "The 2020-12 meta-schema and its vocabulary meta-schemas, shipped inside the
   jar. `$ref`-ing the meta-schema is ordinary practice, and resolving it over
   the network would make validation depend on a working internet connection."
  (delay
    (into {}
          (for [f ["schema" "meta-core" "meta-applicator" "meta-unevaluated"
                   "meta-validation" "meta-meta-data" "meta-format-annotation" "meta-content"]
                :let [doc (json/read-str
                            (slurp (io/resource (str "com/blockether/skjema/meta/2020-12/" f ".json"))))]]
            [(get doc "$id") doc]))))

;; JSON values

(defn- integral?
  "True when the number is an integer VALUE. JSON has one number type, so `1.0`
   is an integer and the specification says so explicitly."
  [x]
  (cond
    (instance? Double x) (let [d (double x)] (and (Double/isFinite d) (== d (Math/rint d))))
    (or (instance? Long x) (instance? Integer x) (instance? java.math.BigInteger x)
        (instance? clojure.lang.BigInt x)) true
    (instance? BigDecimal x) (zero? (.compareTo ^BigDecimal x (.setScale ^BigDecimal x 0 java.math.RoundingMode/DOWN)))
    :else false))

(defn- json-type
  "The specification's type name for a parsed value."
  [x]
  (cond
    (nil? x) "null"
    (instance? Boolean x) "boolean"
    (string? x) "string"
    (map? x) "object"
    (sequential? x) "array"
    (number? x) (if (integral? x) "integer" "number")
    :else "unknown"))

(defn- type-match? [t x]
  (case t
    "integer" (and (number? x) (integral? x))
    "number" (number? x)
    (= t (json-type x))))

(defn- canonical
  "A value in the form JSON equality compares: numbers by VALUE (`1`, `1.0` and
   `1.00` are one value), everything else structurally. `true` is not `1`."
  [x]
  (cond
    (instance? Boolean x) x
    (number? x) [::number (.stripTrailingZeros (bigdec x))]
    (map? x) (persistent! (reduce-kv (fn [m k v] (assoc! m k (canonical v))) (transient {}) x))
    (sequential? x) (mapv canonical x)
    :else x))

(defn- json-equal? [a b] (= (canonical a) (canonical b)))

(defn- num-compare ^long [a b]
  (long (.compareTo (bigdec a) (bigdec b))))

(defn- multiple-of? [x divisor]
  (let [^BigDecimal bx (bigdec x)
        ^BigDecimal bd (bigdec divisor)]
    (and (not (zero? (.signum bd)))
         (try
           (zero? (.compareTo BigDecimal/ZERO (.remainder bx bd)))
           (catch ArithmeticException _ false)))))

(defn- code-point-count ^long [^String s]
  (.codePointCount s 0 (.length s)))

;; The schema index: resources, anchors and dynamic anchors

(def ^:private subschema-keywords
  #{"additionalProperties" "contains" "contentSchema" "else" "if" "items" "not"
    "propertyNames" "then" "unevaluatedItems" "unevaluatedProperties"})

(def ^:private subschema-array-keywords
  #{"allOf" "anyOf" "oneOf" "prefixItems"})

(def ^:private subschema-map-keywords
  #{"$defs" "definitions" "dependencies" "dependentSchemas" "patternProperties"
    "properties"})

(defn- index-schema
  "Record every identifier `schema` declares, descending ONLY through keywords
   that hold schemas. Walking every object instead would read a `$id` inside a
   `const` or an `enum` as if it were an identifier, which it is not."
  [acc schema base ptr]
  (if-not (map? schema)
    acc
    (let [id (get schema "$id")
          resource? (string? id)
          base (if resource? (uri/strip-fragment (uri/resolve-ref base id)) base)
          ptr (if resource? "" ptr)
          entry {:schema schema :base base :ptr ptr}
          acc (cond-> acc
                resource? (assoc-in [:index base] entry))
          acc (if-let [a (get schema "$anchor")]
                (assoc-in acc [:index (str base "#" a)] entry)
                acc)
          acc (if-let [a (get schema "$dynamicAnchor")]
                (-> acc
                    (assoc-in [:index (str base "#" a)] entry)
                    (assoc-in [:dynamic base a] entry))
                acc)]
      (reduce-kv
        (fn [acc k v]
          (let [kptr (str ptr "/" (uri/escape-token k))]
            (cond
              (subschema-keywords k) (index-schema acc v base kptr)

              (and (subschema-array-keywords k) (sequential? v))
              (first (reduce (fn [[acc i] sub]
                               [(index-schema acc sub base (str kptr "/" i)) (inc i)])
                             [acc 0]
                             v))

              (and (subschema-map-keywords k) (map? v))
              (reduce-kv (fn [acc kk sub]
                           (index-schema acc sub base (str kptr "/" (uri/escape-token kk))))
                         acc
                         v)

              :else acc)))
        acc
        schema))))

(defn- pointer-get
  "Walk a JSON Pointer fragment inside one resource, tracking a nested `$id` so
   the answer knows which resource it ended up in."
  [{:keys [schema base ptr]} pointer]
  (loop [node schema base base ptr ptr tokens (uri/pointer-tokens pointer)]
    (if (empty? tokens)
      {:schema node :base base :ptr ptr}
      (let [t (first tokens)
            child (cond
                    (map? node) (get node t ::missing)
                    (sequential? node) (let [i (parse-long t)]
                                         (if (and i (nat-int? i) (< i (count node)))
                                           (nth node i)
                                           ::missing))
                    :else ::missing)]
        (when-not (= ::missing child)
          (let [id (when (map? child) (get child "$id"))
                resource? (string? id)]
            (recur child
                   (if resource? (uri/strip-fragment (uri/resolve-ref base id)) base)
                   (if resource? "" (str ptr "/" (uri/escape-token t)))
                   (rest tokens))))))))

(defn- lookup
  "Find the schema an absolute URI addresses: a whole resource, an anchor, or a
   JSON Pointer inside one."
  [ctx ^String uri]
  (let [uri (if (str/ends-with? uri "#") (subs uri 0 (dec (count uri))) uri)]
    (or (get (:index ctx) uri)
        (let [resource (get (:index ctx) (uri/strip-fragment uri))
              frag (uri/fragment uri)]
          (when resource
            (cond
              (or (nil? frag) (= "" frag)) resource
              (str/starts-with? frag "/") (pointer-get resource frag)
              :else nil))))))

;; Evaluation results

(def ^:private ok {:valid? true :props #{} :items #{} :errors []})

(defn- merge-res [a b]
  {:valid? (and (:valid? a) (:valid? b))
   :props (into (:props a) (:props b))
   :items (into (:items a) (:items b))
   :errors (into (:errors a) (:errors b))})

(defn- child-res
  "A child applicator's result WITHOUT its annotations: what `properties`
   evaluated inside `/foo` says nothing about what was evaluated at `/`."
  [r]
  (assoc r :props #{} :items #{}))

(defn- quiet
  "Keep a subschema's annotations, drop its errors - for a branch whose failure
   is not the schema's failure (`anyOf`, `if`)."
  [r]
  (assoc r :errors []))

(defn- at-keyword
  "Descend into a keyword of the CURRENT schema: both the keyword location from
   the root and the pointer inside the current resource move."
  [ctx & tokens]
  (let [suffix (apply str (map #(str "/" (uri/escape-token (str %))) tokens))]
    (-> ctx
        (update :kw-loc str suffix)
        (update :res-loc str suffix))))

(defn- at-instance [ctx token]
  (update ctx :inst-loc str "/" (uri/escape-token (str token))))

(defn- err [ctx message]
  {:valid? false
   :props #{}
   :items #{}
   :errors [(cond-> {:instanceLocation (:inst-loc ctx)
                     :keywordLocation (:kw-loc ctx)
                     :error message}
              (not (str/blank? (:base ctx)))
              (assoc :absoluteKeywordLocation (str (:base ctx) "#" (:res-loc ctx))))]})

(defn- enter
  "Follow a reference into `target`: the base moves, the pointer inside the
   resource restarts, and the resource joins the DYNAMIC SCOPE that
   `$dynamicRef` searches from the outermost entry inward."
  [ctx target]
  (let [base (:base target)]
    (cond-> (assoc ctx :res-loc (or (:ptr target) "") :id-resolved true)
      (not= base (:base ctx))
      (-> (assoc :base base)
          (update :dyn-scope (fnil conj []) base)))))

;; In-place applicators

(defn- eval-ref [f ctx schema instance]
  (let [ref (get schema "$ref")
        uri (uri/resolve-ref (:base ctx) ref)
        target (lookup ctx uri)]
    (when-not target
      (throw (ex-info (str "cannot resolve $ref " (pr-str ref))
                      {:skjema/error :schema/unresolved-ref :ref ref :base (:base ctx) :uri uri})))
    (f (-> ctx (update :kw-loc str "/$ref") (enter target)) (:schema target) instance)))

(defn- eval-dynamic-ref
  "`$dynamicRef` resolves like `$ref` UNLESS the schema it lands on carries a
   matching `$dynamicAnchor`: then the OUTERMOST resource in the dynamic scope
   that declares that anchor wins. That is how a recursive schema lets the
   caller's own definition take over from the one it was written against."
  [f ctx schema instance]
  (let [ref (get schema "$dynamicRef")
        uri (uri/resolve-ref (:base ctx) ref)
        static (lookup ctx uri)
        frag (uri/fragment uri)
        anchor? (and frag (not (str/starts-with? frag "/")))
        dynamic (when (and anchor?
                           (or (nil? static)
                               (= frag (get (:schema static) "$dynamicAnchor"))))
                  (first (keep #(get-in (:dynamic ctx) [% frag]) (:dyn-scope ctx))))
        target (or dynamic static)]
    (when-not target
      (throw (ex-info (str "cannot resolve $dynamicRef " (pr-str ref))
                      {:skjema/error :schema/unresolved-ref :ref ref :base (:base ctx) :uri uri})))
    (f (-> ctx (update :kw-loc str "/$dynamicRef") (enter target)) (:schema target) instance)))

(defn- eval-all-of [f ctx schema instance]
  (reduce merge-res ok
          (map-indexed (fn [i sub] (f (at-keyword ctx "allOf" i) sub instance))
                       (get schema "allOf"))))

(defn- eval-any-of [f ctx schema instance]
  (let [rs (map-indexed (fn [i sub] (f (at-keyword ctx "anyOf" i) sub instance))
                        (get schema "anyOf"))
        good (filter :valid? rs)]
    (if (seq good)
      (reduce merge-res ok (map quiet good))
      (reduce merge-res
              (err (at-keyword ctx "anyOf") "the instance matches none of the anyOf subschemas")
              rs))))

(defn- eval-one-of [f ctx schema instance]
  (let [rs (map-indexed (fn [i sub] (f (at-keyword ctx "oneOf" i) sub instance))
                        (get schema "oneOf"))
        good (filter :valid? rs)]
    (cond
      (= 1 (count good)) (quiet (first good))
      (empty? good) (reduce merge-res
                            (err (at-keyword ctx "oneOf") "the instance matches none of the oneOf subschemas")
                            rs)
      :else (err (at-keyword ctx "oneOf")
                 (str "the instance matches " (count good) " oneOf subschemas, exactly one is allowed")))))

(defn- eval-not [f ctx schema instance]
  (if (:valid? (f (at-keyword ctx "not") (get schema "not") instance))
    (err (at-keyword ctx "not") "the instance matches the not subschema")
    ok))

(defn- eval-conditional [f ctx schema instance]
  (if-not (contains? schema "if")
    ok
    (let [r (f (at-keyword ctx "if") (get schema "if") instance)]
      (if (:valid? r)
        (if (contains? schema "then")
          (merge-res (quiet r) (f (at-keyword ctx "then") (get schema "then") instance))
          (quiet r))
        (if (contains? schema "else")
          (f (at-keyword ctx "else") (get schema "else") instance)
          ok)))))

(defn- eval-dependent-schemas [f ctx schema instance]
  (if-not (map? instance)
    ok
    (reduce merge-res ok
            (for [[k sub] (get schema "dependentSchemas")
                  :when (contains? instance k)]
              (f (at-keyword ctx "dependentSchemas" k) sub instance)))))

(defn- eval-dependencies
  "The `dependencies` of draft-07, which 2020-12 split in two: an array of
   names is `dependentRequired`, a schema is `dependentSchemas`. It is not a
   2020-12 keyword and never asserts on its own here - it asserts because a
   dialect that defines it said so, or because a 2020-12 schema written before
   the split still means it."
  [f ctx schema instance]
  (if-not (map? instance)
    ok
    (reduce merge-res ok
            (for [[k v] (get schema "dependencies")
                  :when (contains? instance k)]
              (if (sequential? v)
                (let [missing (remove #(contains? instance %) v)]
                  (if (seq missing)
                    (err (at-keyword ctx "dependencies" k)
                         (str "property " (pr-str k) " requires "
                              (str/join ", " (map pr-str missing))))
                    ok))
                (f (at-keyword ctx "dependencies" k) v instance))))))

(defn- eval-format
  "`format` as an ASSERTION, which happens only where the format-assertion
   vocabulary is in force or the caller asked for it. Everywhere else the
   keyword is an annotation and this is never reached."
  [ctx schema instance]
  (let [f (get schema "format")]
    (if (fmt/valid? f instance)
      ok
      (err (at-keyword ctx "format") (str "the string is not a valid " f)))))

;; Child applicators

(defn- eval-object-applicators
  "`properties`, `patternProperties` and `additionalProperties` together,
   because `additionalProperties` is defined as 'whatever the other two did not
   match' - and only its ADJACENT siblings count, never what a `$ref` matched."
  [f ctx schema instance]
  (if-not (map? instance)
    ok
    (let [props (get schema "properties")
          patterns (get schema "patternProperties")
          ks (keys instance)
          named (when (map? props) (filter #(contains? props %) ks))
          matched-pattern (when (map? patterns)
                            (for [k ks [p _] patterns :when (re-find (regex/pattern-of p) k)] k))
          matched (set (concat named matched-pattern))
          additional? (contains? schema "additionalProperties")
          extra (when additional? (remove matched ks))
          rs (concat
               (for [k named]
                 (child-res (f (-> ctx (at-keyword "properties" k) (at-instance k))
                               (get props k)
                               (get instance k))))
               (for [k ks [p sub] patterns :when (re-find (regex/pattern-of p) k)]
                 (child-res (f (-> ctx (at-keyword "patternProperties" p) (at-instance k))
                               sub
                               (get instance k))))
               (for [k extra]
                 (child-res (f (-> ctx (at-keyword "additionalProperties") (at-instance k))
                               (get schema "additionalProperties")
                               (get instance k)))))]
      (assoc (reduce merge-res ok rs) :props (into matched extra)))))

(defn- eval-property-names [f ctx schema instance]
  (if-not (and (contains? schema "propertyNames") (map? instance))
    ok
    (reduce merge-res ok
            (for [k (keys instance)]
              (child-res (f (-> ctx (at-keyword "propertyNames") (at-instance k))
                            (get schema "propertyNames")
                            k))))))

(defn- eval-array-applicators
  "`prefixItems` covers the first N positions, `items` covers everything after
   them. The indices they touched are what `unevaluatedItems` later subtracts."
  [f ctx schema instance]
  (if-not (sequential? instance)
    ok
    (let [prefix (get schema "prefixItems")
          n (count instance)
          pre-n (if (sequential? prefix) (min (count prefix) n) 0)
          items? (contains? schema "items")
          rs (concat
               (for [i (range pre-n)]
                 (child-res (f (-> ctx (at-keyword "prefixItems" i) (at-instance i))
                               (nth prefix i)
                               (nth instance i))))
               (when items?
                 (for [i (range pre-n n)]
                   (child-res (f (-> ctx (at-keyword "items") (at-instance i))
                                 (get schema "items")
                                 (nth instance i))))))]
      (assoc (reduce merge-res ok rs)
             :items (into (set (range pre-n)) (when items? (range pre-n n)))))))

(defn- eval-contains
  "`contains` is the one child applicator whose ANNOTATION is the interesting
   part: the indices that matched are evaluated, which is what keeps
   `unevaluatedItems` from rejecting them."
  [ctx-count-only f ctx schema instance]
  (if-not (and (contains? schema "contains") (sequential? instance))
    ok
    (let [sub (get schema "contains")
          matched (set (for [i (range (count instance))
                             :when (:valid? (f (-> ctx (at-keyword "contains") (at-instance i))
                                               sub
                                               (nth instance i)))]
                         i))
          hits (count matched)
          minc (get schema "minContains")
          maxc (get schema "maxContains")
          minc (if (and ctx-count-only (number? minc)) (long minc) 1)
          maxc (when (and ctx-count-only (number? maxc)) (long maxc))
          low (when (< hits minc)
                (err (at-keyword ctx "contains")
                     (str "only " hits " of " (count instance)
                          " items match contains, at least " minc " must")))
          high (when (and maxc (> hits maxc))
                 (err (at-keyword ctx "maxContains")
                      (str hits " items match contains, at most " maxc " may")))]
      (assoc (reduce merge-res ok (remove nil? [low high])) :items matched))))

(defn- eval-unevaluated-properties [f ctx schema instance evaluated]
  (if-not (and (contains? schema "unevaluatedProperties") (map? instance))
    ok
    (let [extra (remove evaluated (keys instance))
          rs (for [k extra]
               (child-res (f (-> ctx (at-keyword "unevaluatedProperties") (at-instance k))
                             (get schema "unevaluatedProperties")
                             (get instance k))))]
      (assoc (reduce merge-res ok rs) :props (set extra)))))

(defn- eval-unevaluated-items [f ctx schema instance evaluated]
  (if-not (and (contains? schema "unevaluatedItems") (sequential? instance))
    ok
    (let [extra (remove evaluated (range (count instance)))
          rs (for [i extra]
               (child-res (f (-> ctx (at-keyword "unevaluatedItems") (at-instance i))
                             (get schema "unevaluatedItems")
                             (nth instance i))))]
      (assoc (reduce merge-res ok rs) :items (set extra)))))

;; Assertions (the validation vocabulary)

(defn- eval-assertions [ctx schema instance]
  (let [checks
        [(when-let [t (get schema "type")]
           (let [types (if (sequential? t) t [t])]
             (when-not (some #(type-match? % instance) types)
               (err (at-keyword ctx "type")
                    (str "expected " (str/join " or " types) ", got " (json-type instance))))))

         (when (contains? schema "enum")
           (when-not (some #(json-equal? % instance) (get schema "enum"))
             (err (at-keyword ctx "enum") "the instance is not one of the enumerated values")))

         (when (contains? schema "const")
           (when-not (json-equal? (get schema "const") instance)
             (err (at-keyword ctx "const")
                  (str "the instance is not the constant " (json/write-str (get schema "const"))))))

         (when (and (number? instance) (number? (get schema "multipleOf")))
           (when-not (multiple-of? instance (get schema "multipleOf"))
             (err (at-keyword ctx "multipleOf")
                  (str instance " is not a multiple of " (get schema "multipleOf")))))

         (when (and (number? instance) (number? (get schema "maximum")))
           (when (pos? (num-compare instance (get schema "maximum")))
             (err (at-keyword ctx "maximum")
                  (str instance " is greater than the maximum " (get schema "maximum")))))

         (when (and (number? instance) (number? (get schema "exclusiveMaximum")))
           (when-not (neg? (num-compare instance (get schema "exclusiveMaximum")))
             (err (at-keyword ctx "exclusiveMaximum")
                  (str instance " is not below the exclusive maximum " (get schema "exclusiveMaximum")))))

         (when (and (number? instance) (number? (get schema "minimum")))
           (when (neg? (num-compare instance (get schema "minimum")))
             (err (at-keyword ctx "minimum")
                  (str instance " is less than the minimum " (get schema "minimum")))))

         (when (and (number? instance) (number? (get schema "exclusiveMinimum")))
           (when-not (pos? (num-compare instance (get schema "exclusiveMinimum")))
             (err (at-keyword ctx "exclusiveMinimum")
                  (str instance " is not above the exclusive minimum " (get schema "exclusiveMinimum")))))

         (when (and (string? instance) (number? (get schema "maxLength")))
           (when (> (code-point-count instance) (long (get schema "maxLength")))
             (err (at-keyword ctx "maxLength")
                  (str "the string is " (code-point-count instance)
                       " characters long, the maximum is " (get schema "maxLength")))))

         (when (and (string? instance) (number? (get schema "minLength")))
           (when (< (code-point-count instance) (long (get schema "minLength")))
             (err (at-keyword ctx "minLength")
                  (str "the string is " (code-point-count instance)
                       " characters long, the minimum is " (get schema "minLength")))))

         (when (and (string? instance) (string? (get schema "pattern")))
            (when-not (re-find (regex/pattern-of (get schema "pattern")) instance)
             (err (at-keyword ctx "pattern")
                  (str "the string does not match the pattern " (pr-str (get schema "pattern"))))))

         (when (and (sequential? instance) (number? (get schema "maxItems")))
           (when (> (count instance) (long (get schema "maxItems")))
             (err (at-keyword ctx "maxItems")
                  (str "the array has " (count instance) " items, the maximum is " (get schema "maxItems")))))

         (when (and (sequential? instance) (number? (get schema "minItems")))
           (when (< (count instance) (long (get schema "minItems")))
             (err (at-keyword ctx "minItems")
                  (str "the array has " (count instance) " items, the minimum is " (get schema "minItems")))))

         (when (and (sequential? instance) (true? (get schema "uniqueItems")))
           (let [canon (mapv canonical instance)]
             (when-not (= (count canon) (count (set canon)))
               (err (at-keyword ctx "uniqueItems") "the array has duplicate items"))))

         (when (and (map? instance) (number? (get schema "maxProperties")))
           (when (> (count instance) (long (get schema "maxProperties")))
             (err (at-keyword ctx "maxProperties")
                  (str "the object has " (count instance) " properties, the maximum is "
                       (get schema "maxProperties")))))

         (when (and (map? instance) (number? (get schema "minProperties")))
           (when (< (count instance) (long (get schema "minProperties")))
             (err (at-keyword ctx "minProperties")
                  (str "the object has " (count instance) " properties, the minimum is "
                       (get schema "minProperties")))))

         (when (and (map? instance) (sequential? (get schema "required")))
           (let [missing (remove #(contains? instance %) (get schema "required"))]
             (when (seq missing)
               (err (at-keyword ctx "required")
                    (str "missing required " (if (next missing) "properties " "property ")
                         (str/join ", " (map pr-str missing)))))))

         (when (and (map? instance) (map? (get schema "dependentRequired")))
           (let [missing (for [[k required] (get schema "dependentRequired")
                               :when (contains? instance k)
                               r required
                               :when (not (contains? instance r))]
                           [k r])]
             (when (seq missing)
               (err (at-keyword ctx "dependentRequired")
                    (str/join ", " (for [[k r] missing]
                                     (str "property " (pr-str k) " requires " (pr-str r))))))))]]
    (reduce merge-res ok (remove nil? checks))))

;; The evaluator

(defn- vocabulary-validation?
  "Whether the validation vocabulary applies here. A schema resource may declare
   its own meta-schema, and a meta-schema that leaves the validation vocabulary
   out turns `minimum`, `type` and the rest into annotations that assert
   nothing."
  [ctx meta-schema-uri]
  (if (= meta-schema-uri official-meta-schema)
    true
    (if-let [meta (:schema (lookup ctx meta-schema-uri))]
      (let [vocab (get meta "$vocabulary")]
        (if (map? vocab) (contains? vocab validation-vocabulary) true))
      true)))

(defn- vocabulary-format?
  "Whether `format` asserts here. It is an annotation in the dialect every
   schema gets by default; a meta-schema that declares the format-assertion
   vocabulary is asking for the assertion, and the boolean it declares only
   tells an implementation that does not have it what to do."
  [ctx meta-schema-uri]
  (boolean
    (when-let [meta (:schema (lookup ctx meta-schema-uri))]
      (let [vocab (get meta "$vocabulary")]
        (and (map? vocab) (contains? vocab format-assertion-vocabulary))))))

(def ^:private draft-2019-09-keywords
  #{"$anchor" "$comment" "$defs" "$id" "$recursiveAnchor" "$recursiveRef" "$ref"
    "$schema" "$vocabulary" "additionalItems" "additionalProperties" "allOf" "anyOf"
    "const" "contains" "contentEncoding" "contentMediaType" "contentSchema" "default"
    "definitions" "dependencies" "dependentRequired" "dependentSchemas" "deprecated"
    "description" "else" "enum" "examples" "exclusiveMaximum" "exclusiveMinimum"
    "format" "if" "items" "maxContains" "maxItems" "maxLength" "maxProperties"
    "maximum" "minContains" "minItems" "minLength" "minProperties" "minimum"
    "multipleOf" "not" "oneOf" "pattern" "patternProperties" "properties"
    "propertyNames" "readOnly" "required" "then" "title" "type" "unevaluatedItems"
    "unevaluatedProperties" "uniqueItems" "writeOnly"})

(def ^:private draft-07-keywords
  (disj draft-2019-09-keywords "$anchor" "$defs" "$recursiveAnchor" "$recursiveRef"
        "$vocabulary" "contentSchema" "dependentRequired" "dependentSchemas"
        "deprecated" "maxContains" "minContains" "unevaluatedItems"
        "unevaluatedProperties"))

(def ^:private draft-06-keywords
  (disj draft-07-keywords "$comment" "contentEncoding" "contentMediaType" "else" "if"
        "readOnly" "then" "writeOnly"))

(def ^:private draft-04-keywords
  (disj draft-06-keywords "const" "contains" "examples" "propertyNames"))

(def ^:private dialects
  "The keywords each historic draft defines. A resource that declares one of
   these is read as that draft: `prefixItems` inside a 2019-09 resource is not
   a keyword at all but an annotation, which is exactly what a reference from
   a 2020-12 schema into an older document must see. Identifiers are still
   read as `$id`, and `$recursiveRef` is recognized as a keyword without being
   followed - what this library evaluates is the 2020-12 dialect."
  {"https://json-schema.org/draft/2019-09/schema" draft-2019-09-keywords
   "http://json-schema.org/draft-07/schema" draft-07-keywords
   "http://json-schema.org/draft-06/schema" draft-06-keywords
   "http://json-schema.org/draft-04/schema" draft-04-keywords})

(defn- dialect-view
  "A historic schema in 2020-12 spelling: keywords its draft does not define
   are dropped, an `items` holding an array is the `prefixItems` it became,
   `additionalItems` is the `items` that covers the rest, and the draft-04
   booleans `exclusiveMaximum` and `exclusiveMinimum` are the bounds 2020-12
   spells with those names."
  [keywords schema]
  (let [schema (select-keys schema (filter keywords (keys schema)))
        items (get schema "items")
        tuple? (sequential? items)]
    (cond-> schema
      tuple? (-> (dissoc "items") (assoc "prefixItems" items))
      (and tuple? (contains? schema "additionalItems"))
      (-> (dissoc "additionalItems") (assoc "items" (get schema "additionalItems")))
      (true? (get schema "exclusiveMaximum"))
      (-> (dissoc "exclusiveMaximum" "maximum")
          (cond-> (contains? schema "maximum")
            (assoc "exclusiveMaximum" (get schema "maximum"))))
      (true? (get schema "exclusiveMinimum"))
      (-> (dissoc "exclusiveMinimum" "minimum")
          (cond-> (contains? schema "minimum")
            (assoc "exclusiveMinimum" (get schema "minimum")))))))

(defn- eval-schema
  [ctx schema instance]
  (cond
    (true? schema) ok
    (false? schema) (err ctx "the false schema rejects every instance")

    (map? schema)
    (let [depth (inc (long (:depth ctx 0)))
          _ (when (> depth max-eval-depth)
              (throw (ex-info "schema recursion did not terminate"
                              {:skjema/error :schema/recursion :keywordLocation (:kw-loc ctx)})))
          id (get schema "$id")
          ctx (assoc ctx :depth depth)
          ;; A resource reached through a reference is ALREADY at its canonical
          ;; base: resolving its own `$id` a second time would append the
          ;; relative identifier to itself (`nested/nested/foo.json`).
          ctx (if (:id-resolved ctx)
                (dissoc ctx :id-resolved)
                (cond-> ctx
                  (string? id)
                  (as-> c (let [b (uri/strip-fragment (uri/resolve-ref (:base c) id))]
                            (-> c
                                (assoc :base b :res-loc "")
                                (update :dyn-scope (fnil conj []) b))))))
          ctx (if-let [ms (get schema "$schema")]
                (assoc ctx
                       :validation? (vocabulary-validation? ctx ms)
                       :format? (or (:format-assertion? ctx) (vocabulary-format? ctx ms))
                       :dialect (dialects (uri/strip-fragment ms)))
                ctx)
          schema (if-let [keywords (:dialect ctx)] (dialect-view keywords schema) schema)
          parts (cond-> []
                  (string? (get schema "$ref")) (conj (eval-ref eval-schema ctx schema instance))
                  (string? (get schema "$dynamicRef")) (conj (eval-dynamic-ref eval-schema ctx schema instance))
                  (sequential? (get schema "allOf")) (conj (eval-all-of eval-schema ctx schema instance))
                  (sequential? (get schema "anyOf")) (conj (eval-any-of eval-schema ctx schema instance))
                  (sequential? (get schema "oneOf")) (conj (eval-one-of eval-schema ctx schema instance))
                  (contains? schema "not") (conj (eval-not eval-schema ctx schema instance))
                  (contains? schema "if") (conj (eval-conditional eval-schema ctx schema instance))
                  (map? (get schema "dependentSchemas")) (conj (eval-dependent-schemas eval-schema ctx schema instance))
                  (map? (get schema "dependencies")) (conj (eval-dependencies eval-schema ctx schema instance))
                  true (conj (eval-object-applicators eval-schema ctx schema instance))
                  true (conj (eval-property-names eval-schema ctx schema instance))
                  true (conj (eval-array-applicators eval-schema ctx schema instance))
                  true (conj (eval-contains (:validation? ctx true) eval-schema ctx schema instance))
                  (:validation? ctx true) (conj (eval-assertions ctx schema instance))
                  (and (:format? ctx) (string? (get schema "format")))
                  (conj (eval-format ctx schema instance)))
          merged (reduce merge-res ok parts)
          merged (merge-res merged (eval-unevaluated-properties eval-schema ctx schema instance (:props merged)))]
      (merge-res merged (eval-unevaluated-items eval-schema ctx schema instance (:items merged))))

    :else (throw (ex-info "a JSON Schema must be an object or a boolean"
                          {:skjema/error :schema/invalid :value schema}))))

;; Public entry points

(defn compile
  "Index a schema once so every `$ref`, `$anchor` and `$dynamicAnchor` in it is
   already resolved when validation runs.

   Options:
     `:base`     the URI the schema is considered to have been retrieved from
     `:registry` a map of absolute URI -> schema for documents this one
                 references. Nothing is fetched; a reference to a document the
                 registry does not carry fails when it is followed.
     `:format-assertion` make `format` assert instead of annotate. The
                 specification leaves that to the caller unless a meta-schema
                 declares the format-assertion vocabulary, which turns it on
                 for that resource whatever this option says."
  ([schema] (compile schema nil))
  ([schema opts]
   (let [base (uri/strip-fragment (or (:base opts) ""))
         registry (merge @bundled-meta-schemas (:registry opts))
         acc (reduce-kv (fn [acc uri doc]
                          (-> acc
                              (assoc-in [:index uri] {:schema doc :base uri :ptr ""})
                              (index-schema doc uri "")))
                        {:index {} :dynamic {}}
                        registry)
         acc (index-schema acc schema base "")
         root-base (if (and (map? schema) (string? (get schema "$id")))
                     (uri/strip-fragment (uri/resolve-ref base (get schema "$id")))
                     base)
         acc (update acc :index #(assoc % base {:schema schema :base root-base :ptr ""}
                                          root-base {:schema schema :base root-base :ptr ""}))]
     {:skjema/compiled true
      :schema schema
      :base root-base
      :index (:index acc)
       :dynamic (:dynamic acc)
       :format-assertion (boolean (:format-assertion opts))})))

(defn compiled? [x] (boolean (:skjema/compiled x)))

(defn validate
  "Validate `instance` against a compiled schema (or a raw one, compiled on the
   spot) and answer the specification's BASIC output: `{:valid true}`, or
   `{:valid false :errors [...]}` where every error carries `:instanceLocation`,
   `:keywordLocation`, `:absoluteKeywordLocation` and a human `:error`. The keys
   are the specification's own, so writing the answer out as JSON is one call."
  ([schema instance] (validate schema instance nil))
  ([schema instance opts]
   (let [c (if (compiled? schema) schema (compile schema opts))
         ctx {:index (:index c)
              :dynamic (:dynamic c)
              :base (:base c)
              :dyn-scope [(:base c)]
               :validation? true
               :format-assertion? (:format-assertion c)
               :format? (:format-assertion c)
              :inst-loc ""
              :kw-loc ""
              :res-loc ""
              :depth 0}
         r (eval-schema ctx (:schema c) instance)]
     (if (:valid? r)
       {:valid true}
       {:valid false :errors (vec (:errors r))}))))

(defn valid?
  "True when the instance validates. Use `validate` when the reason matters."
  ([schema instance] (valid? schema instance nil))
  ([schema instance opts] (:valid (validate schema instance opts))))
