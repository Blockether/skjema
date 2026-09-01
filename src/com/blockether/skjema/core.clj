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
  (:require [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (com.blockether.skjema Formats Prose Regex Schemas Schemas$Compiled
                                  Schemas$Refusal Uri)
           (java.math BigDecimal)
           (java.nio.file Path)
           (java.util.function Predicate)))

(defn- source-label [source]
  (cond
    (string? source) "JSON string"
    (instance? Path source) (str source)
    (instance? java.net.URL source) (.toExternalForm ^java.net.URL source)
    :else (str source)))

(defn read-schema
  "Read a JSON Schema from JSON text, a Path, File, URL/resource, InputStream,
   or Reader. Objects keep their JSON string keys."
  [source]
  (when (nil? source)
    (throw (ex-info "cannot read a schema from nil" {:skjema/error :schema/read})))
  (let [input (if (instance? Path source) (.toFile ^Path source) source)
        label (source-label source)]
    (try
      (charred/read-json input)
      (catch Throwable t
        (throw (ex-info (str "could not read schema from " label ": " (ex-message t))
                        {:skjema/error :schema/read :source label}
                        t))))))

(defn- json-str
  "Compact JSON text for a value lifted out of a schema into an error message."
  ^String [value]
  (charred/write-json-str value :escape-unicode false :escape-slash false))

(def ^:private ^:const max-eval-depth
  "A cyclic self-reference can recurse without the instance ever shrinking.
   Data-driven recursion terminates on its own; this bound turns a pathological
   schema into an error a caller can catch."
  2048)

(defrecord Ctx
           ;; `keyword` is a declared FIELD because every descent sets it: a key the
           ;; record does not declare lands in its extension map, which is another
           ;; allocation on the hot path and a slower read when an error names it.
           [index dynamic ref-cache base dyn-scope validation? format-assertion? format?
            annotate? quiet? id-resolved inst-path kw-path res-prefix res-path depth dialect
            keyword dialect-uri])

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
                :let [doc (read-schema
                           (io/resource (str "com/blockether/skjema/meta/2020-12/" f ".json")))]]
            [(get doc "$id") doc]))))

;; JSON values

(defn- json-number? [x]
  (and (number? x)
       (cond
         (instance? Double x) (Double/isFinite (double x))
         (instance? Float x) (Float/isFinite (float x))
         :else true)))

(defn- integral?
  "True when a JSON number has an integer value."
  [x]
  (cond
    (or (instance? Double x) (instance? Float x))
    (let [d (double x)] (and (Double/isFinite d) (== d (Math/rint d))))
    (or (instance? Byte x) (instance? Short x) (instance? Integer x)
        (instance? Long x) (instance? java.math.BigInteger x)
        (instance? clojure.lang.BigInt x)) true
    (instance? BigDecimal x)
    (zero? (.compareTo ^BigDecimal x (.setScale ^BigDecimal x 0 java.math.RoundingMode/DOWN)))
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
    (json-number? x) (if (integral? x) "integer" "number")
    :else "unknown"))

(defn- type-match? [t x]
  (case t
    "integer" (and (json-number? x) (integral? x))
    "number" (json-number? x)
    (= t (json-type x))))

(defn- canonical
  "A value in the form JSON equality compares: numbers by VALUE (`1`, `1.0` and
   `1.00` are one value), everything else structurally. `true` is not `1`."
  [x]
  (cond
    (instance? Boolean x) x
    (json-number? x) [::number (.stripTrailingZeros (bigdec x))]
    (map? x) (persistent! (reduce-kv (fn [m k v] (assoc! m k (canonical v))) (transient {}) x))
    (sequential? x) (mapv canonical x)
    :else x))

(defn- num-compare ^long [a b]
  ;; `1`, `1.0` and `1.00` are one JSON number and only BigDecimal compares all
  ;; of them without losing a digit - but two Longs, or two Doubles, are the
  ;; case that actually turns up, and neither needs one. The Double branch
  ;; compares by < and > rather than `Double/compare` so that -0.0 and 0.0 stay
  ;; the same number, as BigDecimal has them.
  (cond
    (and (instance? Long a) (instance? Long b))
    (Long/compare (long a) (long b))

    (and (instance? Double a) (instance? Double b))
    (let [x (double a) y (double b)]
      (cond (< x y) -1 (> x y) 1 :else 0))

    :else (long (.compareTo (bigdec a) (bigdec b)))))

(defn- json-equal?
  "JSON equality: numbers by VALUE, everything else structurally. Scalars are
   answered where they stand, because canonicalizing a string or a Long into a
   comparable shape allocates more than the comparison saves."
  [a b]
  (cond
    (and (string? a) (string? b)) (.equals ^String a ^String b)
    (and (json-number? a) (json-number? b)) (zero? (num-compare a b))
    (or (nil? a) (nil? b)) (and (nil? a) (nil? b))
    (or (instance? Boolean a) (instance? Boolean b))
    (and (instance? Boolean a) (instance? Boolean b) (= a b))
    (or (string? a) (string? b) (json-number? a) (json-number? b)) false
    :else (= (canonical a) (canonical b))))

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

(def ^:private unevaluated-keywords
  "The two keywords whose meaning is 'whatever the rest of this schema did NOT
   evaluate'. They are the only reason an evaluation has to remember which
   members it touched, so a document that never uses them never pays for it."
  #{"unevaluatedProperties" "unevaluatedItems"})

(def ^:private subschema-keywords
  #{"additionalProperties" "contains" "contentSchema" "else" "if" "items" "not"
    "propertyNames" "then" "unevaluatedItems" "unevaluatedProperties"})

(def ^:private subschema-array-keywords
  #{"allOf" "anyOf" "oneOf" "prefixItems"})

(def ^:private subschema-map-keywords
  #{"$defs" "definitions" "dependencies" "dependentSchemas" "patternProperties"
    "properties"})

;; The keyword mask: what a schema node CARRIES, decided once

(defmacro ^:private has?
  "Whether a node's mask carries this keyword group. A schema node is a map of
   at most a handful of keywords, but the evaluation knows twenty-odd it could
   contain: the mask is how a node stops being asked about the ones it does
   not, once per compiled schema instead of once per instance."
  [m bit]
  `(not (zero? (bit-and (long ~m) ~bit))))

(def ^:private keyword-names
  "Every keyword an evaluation would otherwise have to look up. The position is
   arbitrary but fixed: it names the bit a node's mask lights up AND the slot its
   value is prebuilt into, and the count has to stay inside a long."
  ["$ref" "$dynamicRef" "allOf" "anyOf" "oneOf" "not" "if"
   "dependentSchemas" "dependencies"
   "properties" "patternProperties" "additionalProperties"
   "propertyNames" "prefixItems" "items" "contains"
   "unevaluatedProperties" "unevaluatedItems"
   "format" "$id" "$schema"
   "type" "enum" "const"
   "multipleOf" "maximum" "exclusiveMaximum" "minimum" "exclusiveMinimum"
   "maxLength" "minLength" "pattern"
   "maxItems" "minItems" "uniqueItems"
   "maxProperties" "minProperties" "required" "dependentRequired"])

(def ^:private keyword-mask
  "One bit per keyword, so a node stops being asked about the ones it does not
   carry."
  (zipmap keyword-names (iterate #(bit-shift-left ^long % 1) 1)))

(def ^:private keyword-index
  "Where each keyword's value sits in a node's prebuilt slots."
  (zipmap keyword-names (range)))

(def ^:private slot-count (count keyword-names))

(defmacro ^:private defmask
  "Name the bit a keyword lights up, or the bits a group of them share. A
   keyword `keyword-mask` does not know fails the compilation here rather than
   quietly naming nothing."
  [sym & kws]
  `(def ~(vary-meta sym assoc :private true :const true)
     ~(reduce (fn [acc kw]
                (bit-or (long acc)
                        (long (or (keyword-mask kw)
                                  (throw (ex-info (str "no mask bit for " kw) {:keyword kw}))))))
              0
              kws)))

(defmask m-ref "$ref")
(defmask m-dynamic-ref "$dynamicRef")
(defmask m-all-of "allOf")
(defmask m-any-of "anyOf")
(defmask m-one-of "oneOf")
(defmask m-not "not")
(defmask m-if "if")
(defmask m-dependent-schemas "dependentSchemas")
(defmask m-dependencies "dependencies")
(defmask m-object "properties" "patternProperties" "additionalProperties")
(defmask m-property-names "propertyNames")
(defmask m-array "prefixItems" "items")
(defmask m-contains "contains")
(defmask m-unevaluated-props "unevaluatedProperties")
(defmask m-unevaluated-items "unevaluatedItems")
(defmask m-format "format")
(defmask m-id "$id")
(defmask m-dialect "$schema")
(defmask m-type "type")
(defmask m-enum "enum")
(defmask m-const "const")
(defmask m-multiple-of "multipleOf")
(defmask m-maximum "maximum")
(defmask m-exclusive-maximum "exclusiveMaximum")
(defmask m-minimum "minimum")
(defmask m-exclusive-minimum "exclusiveMinimum")
(defmask m-max-length "maxLength")
(defmask m-min-length "minLength")
(defmask m-pattern "pattern")
(defmask m-max-items "maxItems")
(defmask m-min-items "minItems")
(defmask m-unique-items "uniqueItems")
(defmask m-max-properties "maxProperties")
(defmask m-min-properties "minProperties")
(defmask m-required "required")
(defmask m-dependent-required "dependentRequired")
(defmask m-numbers "multipleOf" "maximum" "exclusiveMaximum" "minimum" "exclusiveMinimum")
(defmask m-strings "maxLength" "minLength" "pattern")
(defmask m-arrays "maxItems" "minItems" "uniqueItems")
(defmask m-objects "maxProperties" "minProperties" "required" "dependentRequired")
(defmask m-assertions
  "type" "enum" "const"
  "multipleOf" "maximum" "exclusiveMaximum" "minimum" "exclusiveMinimum"
  "maxLength" "minLength" "pattern"
  "maxItems" "minItems" "uniqueItems"
  "maxProperties" "minProperties" "required" "dependentRequired")

(defmask m-additional-props "additionalProperties")
(defmask m-items "items")

(defmacro ^:private defslot
  "Name the slot a keyword's value is prebuilt into, the way `defmask` names its
   bit. Reading a keyword is then an array index and not a scan of the node's own
   string keys, which is what a map of the schema costs on every instance."
  [sym kw]
  `(def ~(vary-meta sym assoc :private true :const true)
     ~(or (keyword-index kw)
          (throw (ex-info (str "no slot for " kw) {:keyword kw})))))

(defslot s-properties "properties")
(defslot s-pattern-properties "patternProperties")
(defslot s-additional-properties "additionalProperties")
(defslot s-prefix-items "prefixItems")
(defslot s-items "items")
(defslot s-type "type")
(defslot s-enum "enum")
(defslot s-const "const")
(defslot s-multiple-of "multipleOf")
(defslot s-maximum "maximum")
(defslot s-exclusive-maximum "exclusiveMaximum")
(defslot s-minimum "minimum")
(defslot s-exclusive-minimum "exclusiveMinimum")
(defslot s-max-length "maxLength")
(defslot s-min-length "minLength")
(defslot s-pattern "pattern")
(defslot s-max-items "maxItems")
(defslot s-min-items "minItems")
(defslot s-unique-items "uniqueItems")
(defslot s-max-properties "maxProperties")
(defslot s-min-properties "minProperties")
(defslot s-required "required")
(defslot s-dependent-required "dependentRequired")

(defn- compute-mask
  "The mask of a node read straight off its keys."
  ^long [schema]
  (reduce-kv (fn [^long acc k _]
               (if-let [bit (keyword-mask k)] (bit-or acc (long bit)) acc))
             0
             schema))

(def ^:private no-slots
  "The prebuilt values of a node that carries no keyword at all - shared, because
   a map inside `properties` or `enum` is walked like any other node and would
   otherwise get an array of its own."
  (object-array slot-count))

(defn- compute-slots
  "A node's keyword values in one array. Evaluation then reads a keyword by
   position instead of scanning the node's own string keys once per keyword, and
   the array is built where every other per-node fact is: at compile time."
  ^objects [schema]
  (let [^objects a (object-array slot-count)]
    (reduce-kv (fn [^objects a k v]
                 (when-some [i (keyword-index k)] (aset a (int i) v))
                 a)
               a
               schema)))

(defn- mask
  "The mask `with-masks` attached, or the one this node has to be read for -
   a node the compiler never saw, such as the view a legacy dialect makes."
  ^long [schema]
  (let [m (::mask (meta schema))]
    (if m (long m) (compute-mask schema))))

(defn- slots
  "The values `with-masks` prebuilt, or the ones a node the compiler never saw
   has to be read for."
  ^objects [schema]
  (or (::slots (meta schema)) (compute-slots schema)))

(defn- with-masks
  "Attach every node's mask and prebuilt keyword values ONCE, when the schema is
   compiled. Nothing else in the document changes, so a mask on a value that is
   not a schema - inside an `enum`, say - is paid for at compile time and never
   read."
  [x]
  (cond
    (map? x)
    (let [node (persistent! (reduce-kv (fn [acc k v] (assoc! acc k (with-masks v)))
                                       (transient {})
                                       x))
          m (compute-mask node)]
      (with-meta node (assoc (meta x)
                             ::mask m
                             ::slots (if (zero? m) no-slots (compute-slots node)))))

    (sequential? x) (mapv with-masks x)
    :else x))

(defn- with-fast
  "Attach to every schema node the compiled predicate the fast compiler could
   build for it, or nothing where it refuses. A node that says VALID this way
   has nothing wrong underneath it, so `explain` never walks it to find out -
   and a schema the fast compiler refuses at the root still gets the benefit
   inside, one subschema at a time."
  [x]
  (if-not (map? x)
    x
    (let [node (reduce-kv
                (fn [acc k v]
                  (cond
                    (subschema-keywords k)
                    (assoc acc k (with-fast v))

                    (and (subschema-array-keywords k) (sequential? v))
                    (assoc acc k (mapv with-fast v))

                    (and (subschema-map-keywords k) (map? v))
                    (assoc acc k (update-vals v with-fast))

                    :else acc))
                x
                x)]
      (if-let [p (Schemas/compileValidator node)]
        (vary-meta node assoc ::compiled p)
        node))))
(defn- compile-pattern! [location pattern]
  (when (string? pattern)
    (try
      (Regex/patternOf pattern)
      (catch java.util.regex.PatternSyntaxException t
        (throw (ex-info (str "invalid regular expression at " location ": "
                             (.getDescription t))
                        {:skjema/error :schema/invalid
                         :keywordLocation location
                         :pattern pattern}
                        t))))))

(defn- index-schema
  "Index identifiers and precompile regular expressions in schema-valued nodes."
  [acc schema base ptr]
  (if-not (map? schema)
    acc
    (let [_ (compile-pattern! (str ptr "/pattern") (get schema "pattern"))
          id (get schema "$id")
          resource? (string? id)
          base (if resource? (Uri/stripFragment (Uri/resolveRef base id)) base)
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
         (let [kptr (str ptr "/" (Uri/escapeToken k))]
           (cond
             (subschema-keywords k)
             (index-schema (cond-> acc (unevaluated-keywords k) (assoc :unevaluated? true))
                           v base kptr)

             (and (subschema-array-keywords k) (sequential? v))
             (first (reduce (fn [[acc i] sub]
                              [(index-schema acc sub base (str kptr "/" i)) (inc (long i))])
                            [acc 0]
                            v))

             (and (subschema-map-keywords k) (map? v))
             (reduce-kv (fn [acc kk sub]
                          (when (= k "patternProperties")
                            (compile-pattern! (str kptr "/" (Uri/escapeToken kk)) kk))
                          (index-schema acc sub base (str kptr "/" (Uri/escapeToken kk))))
                        acc
                        v)

             :else acc)))
       acc
       schema))))

(defn- pointer-get
  "Walk a JSON Pointer fragment inside one resource, tracking a nested `$id` so
   the answer knows which resource it ended up in."
  [{:keys [schema base ptr]} pointer]
  (loop [node schema base base ptr ptr tokens (Uri/pointerTokens pointer)]
    (if (empty? tokens)
      {:schema node :base base :ptr ptr}
      (let [t (first tokens)
            child (cond
                    (map? node) (get node t ::missing)
                    (sequential? node) (let [i (parse-long t)]
                                         (if (and i (<= 0 (long i)) (< (long i) (long (count node))))
                                           (nth node (long i))
                                           ::missing))
                    :else ::missing)]
        (when-not (= ::missing child)
          (let [id (when (map? child) (get child "$id"))
                resource? (string? id)]
            (recur child
                   (if resource? (Uri/stripFragment (Uri/resolveRef base id)) base)
                   (if resource? "" (str ptr "/" (Uri/escapeToken t)))
                   (rest tokens))))))))

(defn- lookup
  "Find the schema an absolute URI addresses: a whole resource, an anchor, or a
   JSON Pointer inside one."
  [ctx ^String uri]
  (let [uri (if (str/ends-with? uri "#") (subs uri 0 (dec (count uri))) uri)]
    (or (get (:index ctx) uri)
        (let [resource (get (:index ctx) (Uri/stripFragment uri))
              frag (Uri/fragment uri)]
          (when resource
            (cond
              (or (nil? frag) (= "" frag)) resource
              (str/starts-with? frag "/") (pointer-get resource frag)
              :else nil))))))

;; Evaluation results

(def ^:private ok {:valid? true :props #{} :items #{} :errors []})

(def ^:private nope
  "The verdict a fail-fast run answers instead of an error. `valid?` asks for
   the verdict and nothing else, so the message and the three locations that
   would explain it are never built."
  {:valid? false :props #{} :items #{} :errors []})

(defn- merge-res [a b]
  (cond
    (identical? a ok) b
    (identical? b ok) a
    :else
    (let [pa (:props a) pb (:props b)
          ia (:items a) ib (:items b)
          ea (:errors a) eb (:errors b)]
      {:valid? (and (:valid? a) (:valid? b))
       :props (cond (empty? pb) pa (empty? pa) pb :else (into pa pb))
       :items (cond (empty? ib) ia (empty? ia) ib :else (into ia ib))
       :errors (cond (empty? eb) ea (empty? ea) eb :else (into ea eb))})))

(defn- child-res
  "A child applicator's result WITHOUT its annotations: what `properties`
   evaluated inside `/foo` says nothing about what was evaluated at `/`."
  [r]
  (if (and (empty? (:props r)) (empty? (:items r)))
    r
    (assoc r :props #{} :items #{})))

(defn- clean?
  "Whether the fast validator a subschema carries answers this instance by
   itself. An applicator asks BEFORE it descends, so a member nothing is wrong
   with never pays for the locations a walk would have to name. A run that
   collects annotations still descends: the fast answer carries none."
  [ctx sub instance]
  (and (not (:annotate? ctx))
       (map? sub)
       (when-some [^Predicate p (::compiled (.meta ^clojure.lang.IObj sub))] (.test p instance))))
(defn- dirty-members
  "The members of `instance` the schema's own compiled checks refuse, or nil when
   it has no compiled answer about them. A report told which two of nine members
   are wrong walks into those and never names the rest; a run that collects
   annotations has to visit every member anyway."
  [ctx schema instance]
  (when-not (:annotate? ctx)
    (when-some [^Schemas$Compiled c (::compiled (.meta ^clojure.lang.IObj schema))]
      (.dirty c instance))))

(defn- quiet
  "Keep a subschema's annotations, drop its errors - for a branch whose failure
   is not the schema's failure (`anyOf`, `if`)."
  [r]
  (if (empty? (:errors r)) r (assoc r :errors [])))

(defn- pointer
  "A JSON Pointer, from a resource-relative `prefix` and the tokens walked since
   it. Locations travel as PATHS while evaluation runs and become strings only
   here, where an error has to name where it happened: an instance that
   validates never pays for a string nobody reads."
  ^String [^String prefix tokens]
  (if (zero? (count tokens))
    (or prefix "")
    (let [sb (StringBuilder. (or prefix ""))]
      (reduce (fn [^StringBuilder sb t]
                (-> sb
                    (.append "/")
                    (.append (Uri/escapeToken (if (string? t) t (str t))))))
              sb
              tokens)
      (.toString sb))))

(defn- descend
  "The same context at a new location. A record `assoc` copies every field once
   PER KEY, and a descent moves three or four of them, so the copy is made once
   here instead."
  [ctx kw-path res-path inst-path keyword]
  (->Ctx (:index ctx)
         (:dynamic ctx)
         (:ref-cache ctx)
         (:base ctx)
         (:dyn-scope ctx)
         (:validation? ctx)
         (:format-assertion? ctx)
         (:format? ctx)
         (:annotate? ctx)
         (:quiet? ctx)
         (:id-resolved ctx)
         inst-path
         kw-path
         (:res-prefix ctx)
         res-path
         (:depth ctx)
         (:dialect ctx)
         keyword
         (:dialect-uri ctx)))

(defn- at-keyword
  "Descend into a keyword of the CURRENT schema: both the keyword location from
   the root and the pointer inside the current resource move. The current keyword
   is retained separately so an error is actionable without parsing a pointer."
  ([ctx a]
   (if (:quiet? ctx)
     ctx
     (descend ctx (conj (:kw-path ctx) a) (conj (:res-path ctx) a) (:inst-path ctx) a)))
  ([ctx a b]
   (if (:quiet? ctx)
     ctx
     (descend ctx (conj (:kw-path ctx) a b) (conj (:res-path ctx) a b) (:inst-path ctx) a))))

(defn- at-member
  "Descend into a keyword AND the member of the instance it applies to. An
   applicator always moves both, and moving them apart builds the context twice."
  ([ctx a token]
   (if (:quiet? ctx)
     ctx
     (descend ctx (conj (:kw-path ctx) a) (conj (:res-path ctx) a) (conj (:inst-path ctx) token) a)))
  ([ctx a b token]
   (if (:quiet? ctx)
     ctx
     (descend ctx (conj (:kw-path ctx) a b) (conj (:res-path ctx) a b) (conj (:inst-path ctx) token) a))))

(defn- err* [ctx params message]
  {:valid? false
   :props #{}
   :items #{}
   :errors [(cond-> {:instanceLocation (pointer nil (:inst-path ctx))
                     :keywordLocation (pointer nil (:kw-path ctx))
                     :keyword (or (:keyword ctx) "falseSchema")
                     :params params
                     :error message}
              (not (str/blank? (:base ctx)))
              (assoc :absoluteKeywordLocation
                     (str (:base ctx) "#" (pointer (:res-prefix ctx) (:res-path ctx)))))]})

(defmacro ^:private err
  "One structured error. A macro keeps params and prose unbuilt during the
   allocation-free `validate` path."
  [ctx params message]
  `(let [c# ~ctx]
     (if (:quiet? c#) nope (err* c# ~params ~message))))

(defmacro ^:private and-merge
  "Merge the next result into `res`, unless a fail-fast run already knows the
   verdict. An invalid node stays invalid however much more is evaluated under
   it, so `validate` stops there; `explain` goes on and collects every error."
  [quiet? res & body]
  `(let [r# ~res]
     (if (and ~quiet? (not (:valid? r#)))
       r#
       (merge-res r# (do ~@body)))))

(defn- enter
  "Follow a reference into `target`: the base moves, the pointer inside the
   resource restarts, and the resource joins the DYNAMIC SCOPE that
   `$dynamicRef` searches from the outermost entry inward."
  [ctx target]
  (let [base (:base target)]
    (cond-> (assoc ctx :res-prefix (or (:ptr target) "") :res-path [] :id-resolved true)
      (not= base (:base ctx))
      (-> (assoc :base base)
          (update :dyn-scope (fnil conj []) base)))))

;; In-place applicators

(defn- deeper
  "One step further along a chain of references, refusing the chain that never
   ends. Only a reference can recur forever - every other applicator descends
   into an instance that is strictly smaller - so this is the one place the
   bound has to be paid for."
  [ctx]
  (let [depth (inc (long (:depth ctx 0)))]
    (when (> depth (long max-eval-depth))
      (throw (ex-info "schema recursion did not terminate"
                      {:skjema/error :schema/recursion
                       :keywordLocation (pointer nil (:kw-path ctx))})))
    (assoc ctx :depth depth)))

(defn- eval-ref [f ctx schema instance]
  (let [ref (get schema "$ref")
        base (:base ctx)
        cache (:ref-cache ctx)
        ;; The same handful of references is resolved on every validation of
        ;; every instance, and resolution is string work: cache it per compiled
        ;; schema, keyed by the base it was resolved against.
        target (or (get @cache [base ref])
                   (let [uri (Uri/resolveRef base ref)
                         t (lookup ctx uri)]
                     (when-not t
                       (throw (ex-info (str "cannot resolve $ref " (pr-str ref))
                                       {:skjema/error :schema/unresolved-ref :ref ref :base base :uri uri})))
                     (swap! cache assoc [base ref] t)
                     t))]
    (f (-> ctx deeper (update :kw-path conj "$ref") (enter target)) (:schema target) instance)))

(defn- eval-dynamic-ref
  "`$dynamicRef` resolves like `$ref` UNLESS the schema it lands on carries a
   matching `$dynamicAnchor`: then the OUTERMOST resource in the dynamic scope
   that declares that anchor wins. That is how a recursive schema lets the
   caller's own definition take over from the one it was written against."
  [f ctx schema instance]
  (let [ref (get schema "$dynamicRef")
        uri (Uri/resolveRef (:base ctx) ref)
        static (lookup ctx uri)
        frag (Uri/fragment uri)
        anchor? (and frag (not (str/starts-with? frag "/")))
        dynamic (when (and anchor?
                           (or (nil? static)
                               (= frag (get (:schema static) "$dynamicAnchor"))))
                  (first (keep #(get-in (:dynamic ctx) [% frag]) (:dyn-scope ctx))))
        target (or dynamic static)]
    (when-not target
      (throw (ex-info (str "cannot resolve $dynamicRef " (pr-str ref))
                      {:skjema/error :schema/unresolved-ref :ref ref :base (:base ctx) :uri uri})))
    (f (-> ctx deeper (update :kw-path conj "$dynamicRef") (enter target)) (:schema target) instance)))

(defn- eval-all-of [f ctx schema instance]
  (loop [i 0 subs (seq (get schema "allOf")) res ok]
    (if (or (nil? subs) (and (:quiet? ctx) (not (:valid? res))))
      res
      (recur (inc i) (next subs)
             (merge-res res (f (at-keyword ctx "allOf" i) (first subs) instance))))))

(defn- eval-any-of [f ctx schema instance]
  (let [rs (map-indexed (fn [i sub] (f (at-keyword ctx "anyOf" i) sub instance))
                        (get schema "anyOf"))
        good (filter :valid? rs)]
    (if (seq good)
      (reduce merge-res ok (map quiet good))
      (reduce merge-res
              (err (at-keyword ctx "anyOf") {}
                   "the instance matches none of the anyOf subschemas")
              rs))))

(defn- eval-one-of [f ctx schema instance]
  (let [rs (map-indexed (fn [i sub] [i (f (at-keyword ctx "oneOf" i) sub instance)])
                        (get schema "oneOf"))
        good (filter (comp :valid? second) rs)
        passing (mapv first good)]
    (cond
      (= 1 (count good)) (quiet (second (first good)))
      (empty? good) (reduce merge-res
                            (err (at-keyword ctx "oneOf") {:passingSchemas nil}
                                 "the instance matches none of the oneOf subschemas")
                            (map second rs))
      :else (err (at-keyword ctx "oneOf") {:passingSchemas passing}
                 (str "the instance matches " (count good)
                      " oneOf subschemas, exactly one is allowed")))))

(defn- eval-not [f ctx schema instance]
  (if (:valid? (f (at-keyword ctx "not") (get schema "not") instance))
    (err (at-keyword ctx "not") {} "the instance matches the not subschema")
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
    (reduce-kv (fn [res k sub]
                 (if (contains? instance k)
                   (and-merge (:quiet? ctx) res (f (at-keyword ctx "dependentSchemas" k) sub instance))
                   res))
               ok
               (get schema "dependentSchemas"))))

(defn- eval-dependencies
  "The `dependencies` of draft-07, which 2020-12 split in two."
  [f ctx schema instance]
  (if-not (map? instance)
    ok
    (reduce-kv
     (fn [res k v]
       (if-not (contains? instance k)
         res
         (and-merge (:quiet? ctx) res
                    (if (sequential? v)
                      (reduce (fn [res missing]
                                (merge-res res
                                           (err (at-keyword ctx "dependencies" k)
                                                {:property k
                                                 :missingProperty missing
                                                 :deps (str/join ", " v)
                                                 :depsCount (count v)}
                                                (str "property " (pr-str k)
                                                     " requires " (pr-str missing)))))
                              ok
                              (remove #(contains? instance %) v))
                      (f (at-keyword ctx "dependencies" k) v instance)))))
     ok
     (get schema "dependencies"))))

(defn- eval-format
  "Assert a format only where the format-assertion vocabulary is in force."
  [ctx schema instance]
  (let [f (get schema "format")]
    (if (Formats/valid f instance)
      ok
      (err (at-keyword ctx "format") {:format f}
           (str "the string is not a valid " f)))))

(defn- wording
  "The prose one refused keyword answers: `a` is what the schema said, `b` is what
   the instance is. The walking evaluator and the compiled explainer both word a
   refusal here, so neither can drift away from the other."
  [^String kw a b]
  (case kw
    "type" (Prose/words "expected " (str/join " or " a) ", got " (json-type b))
    "enum" "the instance is not one of the enumerated values"
    "const" (str "the instance is not the constant " (json-str a))
    "multipleOf" (Prose/words b " is not a multiple of " a)
    "maximum" (Prose/words b " is greater than the maximum " a)
    "exclusiveMaximum" (Prose/words b " is not below the exclusive maximum " a)
    "minimum" (Prose/words b " is less than the minimum " a)
    "exclusiveMinimum" (Prose/words b " is not above the exclusive minimum " a)
    "maxLength" (Prose/words "the string is " b " characters long, the maximum is " a)
    "minLength" (Prose/words "the string is " b " characters long, the minimum is " a)
    "pattern" (str "the string does not match the pattern " (pr-str a))
    "maxItems" (Prose/words "the array has " b " items, the maximum is " a)
    "minItems" (Prose/words "the array has " b " items, the minimum is " a)
    "uniqueItems" (Prose/words "items " a " and " b " are duplicates")
    "maxProperties" (Prose/words "the object has " b " properties, the maximum is " a)
    "minProperties" (Prose/words "the object has " b " properties, the minimum is " a)
    "required" (str "missing required property " (pr-str a))
    "dependentRequired" (Prose/words "property " (pr-str a) " requires " (pr-str b))
    "additionalProperties" (Prose/words "additional property " (pr-str a) " is not allowed")
    "falseSchema" "the false schema rejects every instance"))

;; Child applicators

(def ^:private absent
  "What `get` answers for a keyword the schema does not carry, so presence costs
   one lookup instead of a `contains?` beside it."
  (Object.))

(defn- member-property
  "One declared `properties` member, without the annotations it made inside."
  [f ctx sub k v]
  (child-res (f (at-member ctx "properties" k k) sub v true)))

(defn- member-additional
  "One member no property or pattern named: a refusal when the schema forbids
   extras, the subschema it hands them to otherwise."
  [f ctx additional k v]
  (if (false? additional)
    (err (at-keyword ctx "additionalProperties")
         {:additionalProperty k}
         (wording "additionalProperties" k nil))
    (child-res (f (at-member ctx "additionalProperties" k) additional v true))))

(defn- eval-object-applicators
  "Evaluate properties, patternProperties and additionalProperties in one pass."
  [f km ^objects sl ctx schema instance refused]
  (let [props (aget sl s-properties)
        patterns (aget sl s-pattern-properties)
        additional (aget sl s-additional-properties)
        props? (map? props)
        patterns? (and (map? patterns) (seq patterns))
        additional? (has? km m-additional-props)]
    (if-not (and (map? instance) (or props? patterns? additional?))
      ok
      (let [annotate? (:annotate? ctx)
            quiet? (:quiet? ctx)]
        (if-some [dirty (if (vector? refused) refused (dirty-members ctx schema instance))]
          (reduce (fn [res k]
                    (if (and quiet? (not (:valid? res)))
                      (reduced res)
                      (let [v (get instance k)
                            sub (if props? (get props k absent) absent)]
                        (merge-res res (if (identical? absent sub)
                                         (member-additional f ctx additional k v)
                                         (member-property f ctx sub k v))))))
                  ok
                  dirty)
          (loop [es (seq instance)
                 res ok
                 covered (when annotate? (transient #{}))]
            (cond
              (and quiet? (not (:valid? res))) res
              (nil? es) (if annotate? (assoc res :props (persistent! covered)) res)
              :else
              (let [e (first es)
                    k (key e)
                    v (val e)
                    sub (if props? (get props k absent) absent)
                    named? (not (identical? absent sub))
                    res (if (and named? (not (clean? ctx sub v)))
                          (merge-res res (member-property f ctx sub k v))
                          res)
                    matched (when patterns?
                              (reduce-kv (fn [acc p sub]
                                           (if (re-find (Regex/patternOf p) k)
                                             (conj acc [p sub])
                                             acc))
                                         []
                                         patterns))
                    res (if (seq matched)
                          (reduce (fn [res [p sub]]
                                    (if (clean? ctx sub v)
                                      res
                                      (merge-res res (child-res (f (at-member ctx "patternProperties" p k)
                                                                   sub
                                                                   v
                                                                   true)))))
                                  res
                                  matched)
                          res)
                    covered? (or named? (seq matched))
                    res (cond
                          (or (not additional?) covered?) res
                          (clean? ctx additional v) res
                          :else (merge-res res (member-additional f ctx additional k v)))]
                (recur (next es)
                       res
                       (if (and annotate? (or covered? additional?)) (conj! covered k) covered))))))))))

(defn- eval-property-names [f ctx schema instance]
  (let [sub (get schema "propertyNames" absent)]
    (if-not (and (map? instance) (not (identical? absent sub)))
      ok
      (loop [ks (seq (keys instance)) res ok]
        (if (or (nil? ks) (and (:quiet? ctx) (not (:valid? res))))
          res
          (recur (next ks)
                 (merge-res res (child-res (f (at-member ctx "propertyNames" (first ks))
                                              sub
                                              (first ks))))))))))

(defn- eval-array-applicators
  "`prefixItems` covers the first N positions, `items` covers everything after
   them. The indices they touched are what `unevaluatedItems` later subtracts -
   and that set is only built when the document has an `unevaluated*` to spend
   it on."
  [f km ^objects sl ctx schema instance refused]
  (let [prefix (aget sl s-prefix-items)
        items (aget sl s-items)
        items? (has? km m-items)
        prefix? (sequential? prefix)]
    (if-not (and (sequential? instance) (or prefix? items?))
      ok
      (let [n (count instance)
            pre-n (long (if prefix? (min (count prefix) n) 0))
            end (long (if items? n pre-n))
            quiet? (:quiet? ctx)
            item (fn [res ^long i]
                   (let [prefixed? (< i pre-n)
                         sub (if prefixed? (nth prefix i) items)
                         v (nth instance i)
                         r (if prefixed?
                             (f (at-member ctx "prefixItems" i i) sub v true)
                             (f (at-member ctx "items" i) sub v true))]
                     (merge-res res (child-res r))))]
        (if-some [dirty (if (vector? refused) refused (dirty-members ctx schema instance))]
          (reduce (fn [res idx]
                    (if (and quiet? (not (:valid? res)))
                      (reduced res)
                      (item res (long idx))))
                  ok
                  dirty)
          (loop [i 0 res ok]
            (cond
              (and quiet? (not (:valid? res))) res
              (>= i end) (if (:annotate? ctx)
                           (assoc res :items (into #{} (range end)))
                           res)
              :else
              (let [sub (if (< i pre-n) (nth prefix i) items)
                    v (nth instance i)]
                (if (clean? ctx sub v)
                  (recur (inc i) res)
                  (recur (inc i) (item res i)))))))))))

(defn- eval-contains
  "Evaluate contains and retain the matching indices for unevaluatedItems."
  [ctx-count-only f ctx schema instance]
  (let [sub (get schema "contains" absent)]
    (if-not (and (sequential? instance) (not (identical? absent sub)))
      ok
      (let [annotate? (:annotate? ctx)
            n (count instance)
            matched (loop [i 0 acc (when annotate? (transient #{})) hits 0]
                      (if (>= i n)
                        [(when annotate? (persistent! acc)) hits]
                        (if (:valid? (f (at-member ctx "contains" i) sub (nth instance i)))
                          (recur (inc i) (if annotate? (conj! acc i) acc) (inc hits))
                          (recur (inc i) acc hits))))
            hits (long (second matched))
            minc (get schema "minContains")
            maxc (get schema "maxContains")
            minc (if (and ctx-count-only (number? minc)) (long minc) 1)
            maxc (when (and ctx-count-only (number? maxc)) (long maxc))
            low (when (< hits minc)
                  (err (at-keyword ctx "contains")
                       {:minContains minc :actual hits}
                       (str "only " hits " of " n
                            " items match contains, at least " minc " must")))
            high (when (and maxc (> hits maxc))
                   (err (at-keyword ctx "maxContains")
                        {:limit maxc :actual hits}
                        (str hits " items match contains, at most " maxc " may")))
            res (reduce merge-res ok (remove nil? [low high]))]
        (if annotate? (assoc res :items (first matched)) res)))))

(defn- eval-unevaluated-properties [f ctx schema instance evaluated]
  (if-not (and (contains? schema "unevaluatedProperties") (map? instance))
    ok
    (let [extra (remove evaluated (keys instance))
          rs (for [k extra]
               (child-res (f (at-member ctx "unevaluatedProperties" k)
                             (get schema "unevaluatedProperties")
                             (get instance k))))]
      (assoc (reduce merge-res ok rs) :props (set extra)))))

(defn- eval-unevaluated-items [f ctx schema instance evaluated]
  (if-not (and (contains? schema "unevaluatedItems") (sequential? instance))
    ok
    (let [extra (remove evaluated (range (count instance)))
          rs (for [i extra]
               (child-res (f (at-member ctx "unevaluatedItems" i)
                             (get schema "unevaluatedItems")
                             (nth instance i))))]
      (assoc (reduce merge-res ok rs) :items (set extra)))))

;; Assertions (the validation vocabulary)

(defn- assert-numbers [^objects sl ctx instance res]
  (let [divisor (aget sl s-multiple-of)
        maximum (aget sl s-maximum)
        exclusive-max (aget sl s-exclusive-maximum)
        minimum (aget sl s-minimum)
        exclusive-min (aget sl s-exclusive-minimum)
        res (if (and (number? divisor) (not (multiple-of? instance divisor)))
              (merge-res res (err (at-keyword ctx "multipleOf")
                                  {:multipleOf divisor}
                                  (wording "multipleOf" divisor instance)))
              res)
        res (if (and (number? maximum) (pos? (num-compare instance maximum)))
              (merge-res res (err (at-keyword ctx "maximum")
                                  {:comparison "<=" :limit maximum}
                                  (wording "maximum" maximum instance)))
              res)
        res (if (and (number? exclusive-max) (not (neg? (num-compare instance exclusive-max))))
              (merge-res res (err (at-keyword ctx "exclusiveMaximum")
                                  {:comparison "<" :limit exclusive-max}
                                  (wording "exclusiveMaximum" exclusive-max instance)))
              res)
        res (if (and (number? minimum) (neg? (num-compare instance minimum)))
              (merge-res res (err (at-keyword ctx "minimum")
                                  {:comparison ">=" :limit minimum}
                                  (wording "minimum" minimum instance)))
              res)]
    (if (and (number? exclusive-min) (not (pos? (num-compare instance exclusive-min))))
      (merge-res res (err (at-keyword ctx "exclusiveMinimum")
                          {:comparison ">" :limit exclusive-min}
                          (wording "exclusiveMinimum" exclusive-min instance)))
      res)))

(defn- assert-strings [^objects sl ctx instance res]
  (let [max-length (aget sl s-max-length)
        min-length (aget sl s-min-length)
        pattern (aget sl s-pattern)
        length (when (or (number? max-length) (number? min-length))
                 (code-point-count instance))
        res (if (and (number? max-length) (> (long length) (long max-length)))
              (merge-res res (err (at-keyword ctx "maxLength")
                                  {:limit max-length :actual length}
                                  (wording "maxLength" max-length length)))
              res)
        res (if (and (number? min-length) (< (long length) (long min-length)))
              (merge-res res (err (at-keyword ctx "minLength")
                                  {:limit min-length :actual length}
                                  (wording "minLength" min-length length)))
              res)]
    (if (and (string? pattern) (not (re-find (Regex/patternOf pattern) instance)))
      (merge-res res (err (at-keyword ctx "pattern")
                          {:pattern pattern}
                          (wording "pattern" pattern nil)))
      res)))

(defn- duplicate-indices [items]
  (loop [seen {} i 0]
    (when (< i (count items))
      (let [value (nth items i)]
        (if-some [j (get seen value)]
          [i j]
          (recur (assoc seen value i) (inc i)))))))

(defn- assert-arrays [^objects sl ctx instance res]
  (let [max-items (aget sl s-max-items)
        min-items (aget sl s-min-items)
        unique? (true? (aget sl s-unique-items))
        n (when (or (number? max-items) (number? min-items)) (count instance))
        res (if (and (number? max-items) (> (long n) (long max-items)))
              (merge-res res (err (at-keyword ctx "maxItems")
                                  {:limit max-items :actual n}
                                  (wording "maxItems" max-items n)))
              res)
        res (if (and (number? min-items) (< (long n) (long min-items)))
              (merge-res res (err (at-keyword ctx "minItems")
                                  {:limit min-items :actual n}
                                  (wording "minItems" min-items n)))
              res)]
    (if unique?
      (if-let [[i j] (duplicate-indices (mapv canonical instance))]
        (merge-res res (err (at-keyword ctx "uniqueItems")
                            {:i i :j j}
                            (wording "uniqueItems" i j)))
        res)
      res)))

(defn- assert-objects [^objects sl ctx instance res]
  (let [max-props (aget sl s-max-properties)
        min-props (aget sl s-min-properties)
        required (aget sl s-required)
        dependent (aget sl s-dependent-required)
        n (when (or (number? max-props) (number? min-props)) (count instance))
        res (if (and (number? max-props) (> (long n) (long max-props)))
              (merge-res res (err (at-keyword ctx "maxProperties")
                                  {:limit max-props :actual n}
                                  (wording "maxProperties" max-props n)))
              res)
        res (if (and (number? min-props) (< (long n) (long min-props)))
              (merge-res res (err (at-keyword ctx "minProperties")
                                  {:limit min-props :actual n}
                                  (wording "minProperties" min-props n)))
              res)
        res (if (sequential? required)
              (reduce (fn [res missing]
                        (if (contains? instance missing)
                          res
                          (merge-res res
                                     (err (at-keyword ctx "required")
                                          {:missingProperty missing}
                                          (wording "required" missing nil)))))
                      res
                      required)
              res)]
    (if (map? dependent)
      (reduce (fn [res [property required]]
                (reduce (fn [res missing]
                          (if (contains? instance missing)
                            res
                            (merge-res res
                                       (err (at-keyword ctx "dependentRequired")
                                            {:property property
                                             :missingProperty missing
                                             :deps (str/join ", " required)
                                             :depsCount (count required)}
                                            (wording "dependentRequired" property missing)))))
                        res
                        required))
              res
              (filter (fn [[property _]] (contains? instance property)) dependent))
      res)))

(defn- eval-assertions
  "Evaluate the validation vocabulary by the instance's JSON type."
  [^long km ^objects sl ctx instance]
  (let [res (if (has? km m-type)
              (let [t (aget sl s-type)
                    types (if (sequential? t) t [t])]
                (if (some #(type-match? % instance) types)
                  ok
                  (err (at-keyword ctx "type")
                       {:type t}
                       (wording "type" types instance))))
              ok)
        res (if (has? km m-enum)
              (let [values (aget sl s-enum)]
                (if (some #(json-equal? % instance) values)
                  res
                  (merge-res res (err (at-keyword ctx "enum")
                                      {:allowedValues values}
                                      (wording "enum" nil nil)))))
              res)
        res (if (has? km m-const)
              (let [const (aget sl s-const)]
                (if (json-equal? const instance)
                  res
                  (merge-res res (err (at-keyword ctx "const")
                                      {:allowedValue const}
                                      (wording "const" const nil)))))
              res)]
    (cond
      (json-number? instance) (if (has? km m-numbers) (assert-numbers sl ctx instance res) res)
      (string? instance) (if (has? km m-strings) (assert-strings sl ctx instance res) res)
      (map? instance) (if (has? km m-objects) (assert-objects sl ctx instance res) res)
      (sequential? instance) (if (has? km m-arrays) (assert-arrays sl ctx instance res) res)
      :else res)))

;; The compiled explainer
;;
;; A document the fast compiler took carries a closed set of keywords, and every
;; location its errors name is known while it compiles. What the instance is left
;; to pay for is the prose of the errors it actually has, so an invalid instance
;; is explained by one walk that allocates those and nothing else.

(def ^:private unexplainable
  "What a node answers when it cannot be compiled into errors, so the document
   keeps the walking evaluator instead."
  (Object.))

(defn- error-of
  "One structured error, at locations the compiler already knows."
  [^String inst-loc ^String kw-loc ^String kw params message]
  {:instanceLocation inst-loc
   :keywordLocation kw-loc
   :keyword kw
   :params params
   :error message})

(def ^:private ^:const error-limit
  "How many errors one report carries. A reader acts on the first few and the rest
   only cost the instance that has them, so the walk stops once it has this many
   and every path answers the same prefix of the same reasons."
  5)

(defn- report
  "The BASIC answer for an instance that refused, at no more than `error-limit`
   errors."
  [errors]
  (let [errors (if (vector? errors) errors (vec errors))
        n (count errors)]
    (when (pos? n)
      {:valid false
       :errors (if (> n (long error-limit)) (subvec errors 0 error-limit) errors)})))

(defn- member-location ^String [^String loc k]
  (Prose/member loc (Uri/escapeToken (if (string? k) k (str k)))))

(defn- explain-chain
  "The checks of one node as a single function, in the order the walking evaluator
   runs them. Each answers the errors it was handed plus the ones it found."
  [checks]
  (let [checks (vec (remove nil? checks))]
    (case (count checks)
      0 nil
      1 (nth checks 0)
      2 (let [a (nth checks 0)
              b (nth checks 1)]
          (fn [v l out] (b v l (a v l out))))
      (let [^objects fs (into-array clojure.lang.IFn checks)]
        (fn [v l out]
          (areduce fs i acc out (.invoke ^clojure.lang.IFn (aget fs i) v l acc)))))))

(defn- explain-number-assertions [schema ^String kw-loc]
  (let [divisor (get schema "multipleOf")
        maximum (get schema "maximum")
        exclusive-max (get schema "exclusiveMaximum")
        minimum (get schema "minimum")
        exclusive-min (get schema "exclusiveMinimum")
        f (explain-chain
           [(when (number? divisor)
              (let [kl (str kw-loc "/multipleOf")
                    params {:multipleOf divisor}]
                (fn [v l out]
                  (if (multiple-of? v divisor)
                    out
                    (conj! out (error-of l kl "multipleOf" params (wording "multipleOf" divisor v)))))))
            (when (number? maximum)
              (let [kl (str kw-loc "/maximum")
                    params {:comparison "<=" :limit maximum}]
                (fn [v l out]
                  (if (pos? (num-compare v maximum))
                    (conj! out (error-of l kl "maximum" params (wording "maximum" maximum v)))
                    out))))
            (when (number? exclusive-max)
              (let [kl (str kw-loc "/exclusiveMaximum")
                    params {:comparison "<" :limit exclusive-max}]
                (fn [v l out]
                  (if (neg? (num-compare v exclusive-max))
                    out
                    (conj! out (error-of l kl "exclusiveMaximum" params
                                         (wording "exclusiveMaximum" exclusive-max v)))))))
            (when (number? minimum)
              (let [kl (str kw-loc "/minimum")
                    params {:comparison ">=" :limit minimum}]
                (fn [v l out]
                  (if (neg? (num-compare v minimum))
                    (conj! out (error-of l kl "minimum" params (wording "minimum" minimum v)))
                    out))))
            (when (number? exclusive-min)
              (let [kl (str kw-loc "/exclusiveMinimum")
                    params {:comparison ">" :limit exclusive-min}]
                (fn [v l out]
                  (if (pos? (num-compare v exclusive-min))
                    out
                    (conj! out (error-of l kl "exclusiveMinimum" params
                                         (wording "exclusiveMinimum" exclusive-min v)))))))])]
    (when f
      (fn [v l out] (if (json-number? v) (f v l out) out)))))

(defn- explain-string-assertions [schema ^String kw-loc]
  (let [max-length (get schema "maxLength")
        min-length (get schema "minLength")
        pattern (get schema "pattern")
        f (explain-chain
           [(when (number? max-length)
              (let [kl (str kw-loc "/maxLength")]
                (fn [v l out]
                  (let [n (code-point-count v)]
                    (if (> n (long max-length))
                      (conj! out (error-of l kl "maxLength" {:limit max-length :actual n}
                                           (wording "maxLength" max-length n)))
                      out)))))
            (when (number? min-length)
              (let [kl (str kw-loc "/minLength")]
                (fn [v l out]
                  (let [n (code-point-count v)]
                    (if (< n (long min-length))
                      (conj! out (error-of l kl "minLength" {:limit min-length :actual n}
                                           (wording "minLength" min-length n)))
                      out)))))
            (when (string? pattern)
              (let [kl (str kw-loc "/pattern")
                    params {:pattern pattern}
                    p (Regex/patternOf pattern)]
                (fn [v l out]
                  (if (re-find p v)
                    out
                    (conj! out (error-of l kl "pattern" params (wording "pattern" pattern nil)))))))])]
    (when f
      (fn [v l out] (if (string? v) (f v l out) out)))))

(defn- explain-array-assertions [schema ^String kw-loc]
  (let [max-items (get schema "maxItems")
        min-items (get schema "minItems")
        unique? (true? (get schema "uniqueItems"))
        f (explain-chain
           [(when (number? max-items)
              (let [kl (str kw-loc "/maxItems")]
                (fn [v l out]
                  (let [n (count v)]
                    (if (> (long n) (long max-items))
                      (conj! out (error-of l kl "maxItems" {:limit max-items :actual n}
                                           (wording "maxItems" max-items n)))
                      out)))))
            (when (number? min-items)
              (let [kl (str kw-loc "/minItems")]
                (fn [v l out]
                  (let [n (count v)]
                    (if (< (long n) (long min-items))
                      (conj! out (error-of l kl "minItems" {:limit min-items :actual n}
                                           (wording "minItems" min-items n)))
                      out)))))
            (when unique?
              (let [kl (str kw-loc "/uniqueItems")]
                (fn [v l out]
                  (if-let [[i j] (duplicate-indices (mapv canonical v))]
                    (conj! out (error-of l kl "uniqueItems" {:i i :j j} (wording "uniqueItems" i j)))
                    out))))])]
    (when f
      (fn [v l out] (if (sequential? v) (f v l out) out)))))

(defn- explain-object-assertions [schema ^String kw-loc]
  (let [max-props (get schema "maxProperties")
        min-props (get schema "minProperties")
        required (get schema "required")
        dependent (get schema "dependentRequired")
        f (explain-chain
           [(when (number? max-props)
              (let [kl (str kw-loc "/maxProperties")]
                (fn [v l out]
                  (let [n (count v)]
                    (if (> (long n) (long max-props))
                      (conj! out (error-of l kl "maxProperties" {:limit max-props :actual n}
                                           (wording "maxProperties" max-props n)))
                      out)))))
            (when (number? min-props)
              (let [kl (str kw-loc "/minProperties")]
                (fn [v l out]
                  (let [n (count v)]
                    (if (< (long n) (long min-props))
                      (conj! out (error-of l kl "minProperties" {:limit min-props :actual n}
                                           (wording "minProperties" min-props n)))
                      out)))))
            (when (sequential? required)
              (let [kl (str kw-loc "/required")
                    ^objects names (into-array Object required)
                    n (alength names)]
                (fn [v l out]
                  (loop [i 0 out out]
                    (if (== i n)
                      out
                      (let [name (aget names i)]
                        (recur (inc i)
                               (if (contains? v name)
                                 out
                                 (conj! out (error-of l kl "required" {:missingProperty name}
                                                      (wording "required" name nil)))))))))))
            (when (map? dependent)
              (let [kl (str kw-loc "/dependentRequired")
                    groups (mapv (fn [[property names]]
                                   [property
                                    (mapv (fn [missing]
                                            [missing
                                             {:property property
                                              :missingProperty missing
                                              :deps (str/join ", " names)
                                              :depsCount (count names)}
                                             (wording "dependentRequired" property missing)])
                                          names)])
                                 dependent)]
                (fn [v l out]
                  (reduce (fn [out [property misses]]
                            (if-not (contains? v property)
                              out
                              (reduce (fn [out [missing params message]]
                                        (if (contains? v missing)
                                          out
                                          (conj! out (error-of l kl "dependentRequired" params message))))
                                      out
                                      misses)))
                          out
                          groups))))])]
    (if (and (map? dependent) (not (every? sequential? (vals dependent))))
      unexplainable
      (when f
        (fn [v l out] (if (map? v) (f v l out) out))))))

(defn- explain-value-assertions [schema ^String kw-loc]
  (let [t (get schema "type" absent)
        values (get schema "enum" absent)
        const (get schema "const" absent)]
    [(when-not (identical? absent t)
       (let [types (if (sequential? t) t [t])
             kl (str kw-loc "/type")
             params {:type t}
             one (when (string? t) t)]
         (fn [v l out]
           (if (if one (type-match? one v) (some #(type-match? % v) types))
             out
             (conj! out (error-of l kl "type" params (wording "type" types v)))))))
     (when-not (identical? absent values)
       (let [kl (str kw-loc "/enum")
             params {:allowedValues values}
             message (wording "enum" nil nil)]
         (fn [v l out]
           (if (some #(json-equal? % v) values)
             out
             (conj! out (error-of l kl "enum" params message))))))
     (when-not (identical? absent const)
       (let [kl (str kw-loc "/const")
             params {:allowedValue const}
             message (wording "const" const nil)]
         (fn [v l out]
           (if (json-equal? const v)
             out
             (conj! out (error-of l kl "const" params message))))))]))

(defn- explain-object-applicators
  "The declared properties and the extras, over the members the compiled node
   refused. A member nothing is wrong with is never visited: the compiled node
   already answered for it."
  [f schema ^String kw-loc]
  (let [props (get schema "properties")
        additional (get schema "additionalProperties" absent)
        props? (map? props)
        additional? (not (identical? absent additional))
        by-key (when props?
                 (reduce-kv (fn [acc k sub]
                              (if (identical? unexplainable acc)
                                acc
                                (let [child (f sub (str kw-loc "/properties/" (Uri/escapeToken k)) "properties")]
                                  (if (identical? unexplainable child)
                                    unexplainable
                                    (assoc acc k [(str "/" (Uri/escapeToken k)) child])))))
                            {}
                            props))
        extra (when additional?
                (if (false? additional)
                  ::forbidden
                  (f additional (str kw-loc "/additionalProperties") "additionalProperties")))]
    (cond
      (identical? unexplainable by-key) unexplainable
      (identical? unexplainable extra) unexplainable
      (and (empty? by-key) (nil? extra)) nil
      :else
      (let [by-key (or by-key {})
            kl (str kw-loc "/additionalProperties")]
        (fn [v ^String l out refused]
          (if-not (map? v)
            out
            (let [^clojure.lang.PersistentVector refused refused
                  n (.count refused)]
              (loop [i 0 out out]
                (if (and (< i n) (< (count out) error-limit))
                  (let [k (.nth refused i)
                        member (.nth refused (inc i))]
                    (recur (+ i 2)
                           (if-some [entry (get by-key k)]
                             (if-let [child (nth entry 1)]
                               (child member (.concat l ^String (nth entry 0)) out)
                               out)
                             (cond
                               (identical? ::forbidden extra)
                               (conj! out (error-of l kl "additionalProperties"
                                                    {:additionalProperty k}
                                                    (wording "additionalProperties" k nil)))

                               (some? extra)
                               (extra member (member-location l k) out)

                               :else out))))
                  out)))))))))

(defn- explain-array-applicators
  "`prefixItems` covers the first N positions, `items` everything after them, over
   the indices the compiled node refused."
  [f schema ^String kw-loc]
  (let [prefix (get schema "prefixItems")
        items (get schema "items" absent)
        prefix? (sequential? prefix)
        compiled-prefix (when prefix?
                          (mapv (fn [i sub] (f sub (str kw-loc "/prefixItems/" i) "prefixItems"))
                                (range)
                                prefix))
        compiled-items (when-not (identical? absent items)
                         (f items (str kw-loc "/items") "items"))]
    (cond
      (some #(identical? unexplainable %) compiled-prefix) unexplainable
      (identical? unexplainable compiled-items) unexplainable
      (and (nil? compiled-items) (every? nil? compiled-prefix)) nil
      :else
      (let [^objects pre (into-array Object (or compiled-prefix []))
            pre-n (alength pre)]
        (fn [v l out refused]
          (if-not (sequential? v)
            out
            (let [^clojure.lang.PersistentVector refused refused
                  n (.count refused)]
              (loop [i 0 out out]
                (if (and (< i n) (< (count out) error-limit))
                  (let [idx (long (.nth refused i))
                        item (.nth refused (inc i))
                        g (if (< idx pre-n) (aget pre idx) compiled-items)]
                    (recur (+ i 2) (if g (g item (Prose/index l idx) out) out)))
                  out)))))))))

(defn- explain-node
  "The errors one schema node answers, compiled from the node and the location it
   stands at. The compiled node answers FIRST: it says whether anything under it
   refuses at all, which of its members do and whether the level's own checks are
   clean, so the walk that names them steps only where an error is and re-asks
   nothing that already passed. Nil where nothing under it can refuse,
   `unexplainable` where the walking evaluator has to answer instead."
  [schema ^String kw-loc ^String kw]
  (cond
    (true? schema) nil

    (false? schema)
    (let [k (or kw "falseSchema")
          message (wording "falseSchema" nil nil)]
      (fn [_ l out] (conj! out (error-of l kw-loc k {} message))))

    (not (map? schema)) unexplainable

    ;; A node with an identifier moves the base every error under it is named
    ;; from, and an absolute location is not something this compiler tracks.
    (contains? schema "$id") unexplainable

    :else
    (let [object-fn (explain-object-applicators explain-node schema kw-loc)
          array-fn (explain-array-applicators explain-node schema kw-loc)
          value-parts (explain-value-assertions schema kw-loc)
          numbers (explain-number-assertions schema kw-loc)
          strings (explain-string-assertions schema kw-loc)
          objects (explain-object-assertions schema kw-loc)
          arrays (explain-array-assertions schema kw-loc)
          compiled (::compiled (meta schema))]
      (cond
        (some #(identical? unexplainable %)
              (into [object-fn array-fn numbers strings objects arrays] value-parts))
        unexplainable

        (nil? compiled) unexplainable

        :else
        (let [values (explain-chain value-parts)
              ^Schemas$Compiled p compiled]
          (when (or object-fn array-fn values numbers strings objects arrays)
            (fn [v l out]
              (if-some [^Schemas$Refusal r (.refusals p v)]
                (let [refused (.-refused r)
                      members (.-members r)
                      out (if object-fn (object-fn v l out members) out)
                      out (if array-fn (array-fn v l out members) out)
                      out (if (and values (pos? (bit-and refused Schemas/VALUE_CHECKS)))
                            (values v l out)
                            out)
                      out (if (and numbers (pos? (bit-and refused Schemas/NUMBER_CHECKS)))
                            (numbers v l out)
                            out)
                      out (if (and strings (pos? (bit-and refused Schemas/STRING_CHECKS)))
                            (strings v l out)
                            out)
                      out (if (and objects (pos? (bit-and refused Schemas/OBJECT_CHECKS)))
                            (objects v l out)
                            out)]
                  (if (and arrays (pos? (bit-and refused Schemas/ARRAY_CHECKS)))
                    (arrays v l out)
                    out))
                out))))))))

(defn- compiled-explainer
  "The errors of a whole document as a bare function of the instance, or nil where
   the walking evaluator has to answer it."
  [schema]
  (let [f (explain-node schema "" nil)]
    (when-not (or (nil? f) (identical? unexplainable f))
      (fn [instance]
        (report (persistent! (f instance "" (transient []))))))))

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
  ([ctx schema instance] (eval-schema ctx schema instance false))
  ([ctx schema instance refused]
   (cond
     (true? schema) ok
     (false? schema) (err ctx {} (wording "falseSchema" nil nil))

    ;; A node the fast compiler took answers a VALID instance by itself: nothing
    ;; under it can be wrong, so the walk that exists to say what IS wrong never
    ;; runs.
     (and (not refused) (clean? ctx schema instance))
     ok
     (map? schema)
     (let [km (mask schema)
          ;; A resource reached through a reference is ALREADY at its canonical
          ;; base: resolving its own `$id` a second time would append the
          ;; relative identifier to itself (`nested/nested/foo.json`).
           ctx (if (:id-resolved ctx)
                 (assoc ctx :id-resolved false)
                 (let [id (when (has? km m-id) (get schema "$id"))]
                   (if (string? id)
                     (let [b (Uri/stripFragment (Uri/resolveRef (:base ctx) id))]
                       (-> ctx
                           (assoc :base b :res-prefix "" :res-path [])
                           (update :dyn-scope (fnil conj []) b)))
                     ctx)))
           ctx (if-let [ms (when (has? km m-dialect) (get schema "$schema"))]
                 (if (identical? ms (:dialect-uri ctx))
                   ctx
                   (assoc ctx
                          :validation? (vocabulary-validation? ctx ms)
                          :format? (or (:format-assertion? ctx) (vocabulary-format? ctx ms))
                          :dialect (dialects (Uri/stripFragment ms))
                          :dialect-uri ms))
                 ctx)
          ;; A legacy dialect is read through a view of the node, which the
          ;; compiler never saw and whose mask is therefore read here.
           view (if-let [keywords (:dialect ctx)] (dialect-view keywords schema) schema)
           km (if (identical? view schema) km (compute-mask view))
           sl (if (identical? view schema) (slots schema) (compute-slots view))
           schema view
           quiet? (:quiet? ctx)
           validation? (:validation? ctx true)
          ;; Every applicator the schema does NOT carry costs one bit test and
          ;; nothing else, and a fail-fast run stops at the first one that
          ;; refuses: `and-merge` is where both of those happen.
           res (if (and (has? km m-ref) (string? (get schema "$ref")))
                 (and-merge quiet? ok (eval-ref eval-schema ctx schema instance))
                 ok)
           res (if (and (has? km m-dynamic-ref) (string? (get schema "$dynamicRef")))
                 (and-merge quiet? res (eval-dynamic-ref eval-schema ctx schema instance))
                 res)
           res (if (and (has? km m-all-of) (sequential? (get schema "allOf")))
                 (and-merge quiet? res (eval-all-of eval-schema ctx schema instance))
                 res)
           res (if (and (has? km m-any-of) (sequential? (get schema "anyOf")))
                 (and-merge quiet? res (eval-any-of eval-schema ctx schema instance))
                 res)
           res (if (and (has? km m-one-of) (sequential? (get schema "oneOf")))
                 (and-merge quiet? res (eval-one-of eval-schema ctx schema instance))
                 res)
           res (if (has? km m-not)
                 (and-merge quiet? res (eval-not eval-schema ctx schema instance))
                 res)
           res (if (has? km m-if)
                 (and-merge quiet? res (eval-conditional eval-schema ctx schema instance))
                 res)
           res (if (and (has? km m-dependent-schemas) (map? (get schema "dependentSchemas")))
                 (and-merge quiet? res (eval-dependent-schemas eval-schema ctx schema instance))
                 res)
           res (if (and (has? km m-dependencies) (map? (get schema "dependencies")))
                 (and-merge quiet? res (eval-dependencies eval-schema ctx schema instance))
                 res)
           object? (map? instance)
           array? (sequential? instance)
           res (if (and object? (has? km m-object))
                 (and-merge quiet? res (eval-object-applicators eval-schema km sl ctx schema instance refused))
                 res)
           res (if (and object? (has? km m-property-names))
                 (and-merge quiet? res (eval-property-names eval-schema ctx schema instance))
                 res)
           res (if (and array? (has? km m-array))
                 (and-merge quiet? res (eval-array-applicators eval-schema km sl ctx schema instance refused))
                 res)
           res (if (and array? (has? km m-contains))
                 (and-merge quiet? res (eval-contains validation? eval-schema ctx schema instance))
                 res)
           res (if (and validation? (has? km m-assertions))
                 (and-merge quiet? res (eval-assertions km sl ctx instance))
                 res)
           res (if (and (has? km m-format) (:format? ctx) (string? (get schema "format")))
                 (and-merge quiet? res (eval-format ctx schema instance))
                 res)
           res (if (and object? (has? km m-unevaluated-props))
                 (and-merge quiet? res (eval-unevaluated-properties eval-schema ctx schema instance (:props res)))
                 res)]
       (if (and array? (has? km m-unevaluated-items))
         (and-merge quiet? res (eval-unevaluated-items eval-schema ctx schema instance (:items res)))
         res))

     :else (throw (ex-info "a JSON Schema must be an object or a boolean"
                           {:skjema/error :schema/invalid :value schema})))))

;; Public entry points

(defn- eval-ctx
  "The evaluation context a compiled schema is validated with. It depends on
   the SCHEMA and not on the instance, so it is built once, at compile time,
   and every validation starts from the same value. The dialect the ROOT
   declares is read here for the same reason: which vocabularies a
   meta-schema turns on is a fact about the document, and a schema that
   declares one would otherwise pay for that lookup on every instance."
  [c quiet?]
  (let [ctx (->Ctx (:index c)
                   (:dynamic c)
                   (:ref-cache c)
                   (:base c)
                   [(:base c)]
                   true
                   (:format-assertion c)
                   (:format-assertion c)
                   (:annotate? c)
                   quiet?
                   false
                   [] [] "" []
                   0
                   nil
                   nil
                   nil)
        schema (:schema c)
        ms (when (map? schema) (get schema "$schema"))]
    (if (string? ms)
      (assoc ctx
             :validation? (vocabulary-validation? ctx ms)
             :format? (or (:format-assertion? ctx) (vocabulary-format? ctx ms))
             :dialect (dialects (Uri/stripFragment ms))
             :dialect-uri ms)
      ctx)))

(defn compile-schema
  "Compile and index a schema once so repeated validation does not walk its
   structure. Options: `:base`, `:registry`, and `:format-assertion`. Referenced
   documents are resolved only from the supplied registry."
  ([schema] (compile-schema schema nil))
  ([schema opts]
   (let [base (Uri/stripFragment (or (:base opts) ""))
         registry (update-vals (merge @bundled-meta-schemas (:registry opts)) with-masks)
         acc (reduce-kv (fn [acc uri doc]
                          (-> acc
                              (assoc-in [:index uri] {:schema doc :base uri :ptr ""})
                              (index-schema doc uri "")))
                        {:index {} :dynamic {}}
                        registry)
         schema (with-masks schema)
         ;; The fast compiler reads 2020-12 as written. A document that declares
         ;; another dialect is evaluated through a legacy view it never sees, and
         ;; a registry or an asserting `format` is beyond it, so those documents
         ;; stay with the complete evaluator.
         fast? (and (not (:format-assertion opts))
                    (empty? (:registry opts))
                    (contains? #{nil official-meta-schema}
                               (when (map? schema) (get schema "$schema"))))
         schema (if fast? (with-fast schema) schema)
         acc (index-schema acc schema base "")
         root-base (if (and (map? schema) (string? (get schema "$id")))
                     (Uri/stripFragment (Uri/resolveRef base (get schema "$id")))
                     base)
         acc (update acc :index #(assoc % base {:schema schema :base root-base :ptr ""}
                                        root-base {:schema schema :base root-base :ptr ""}))
         c {:skjema/compiled true
            :schema schema
            :base root-base
            :index (:index acc)
            :dynamic (:dynamic acc)
              ;; Annotations exist for `unevaluatedProperties` and
              ;; `unevaluatedItems`. A document that never mentions them has
              ;; nothing to spend a set of evaluated member names on, so none
              ;; is built.
            :annotate? (boolean (:unevaluated? acc))
              ;; `$ref` resolution is string work against a fixed index: the
              ;; same handful of answers, once per compiled schema instead of
              ;; once per instance.
            :ref-cache (atom {})
            :format-assertion (boolean (:format-assertion opts))}
         compiled (assoc c
                         :ctx (eval-ctx c false)
                         :quiet-ctx (eval-ctx c true)
                         :fast-explainer (when (and fast? (str/blank? base))
                                           (compiled-explainer schema)))
         meta-uri (when (map? schema) (or (get schema "$schema") official-meta-schema))
         target (when (= official-meta-schema meta-uri)
                  (get-in compiled [:index meta-uri]))
         checked (when target
                   (eval-schema (-> (:ctx compiled) (enter target)) (:schema target) schema))]
     (when (and checked (not (:valid? checked)))
       (let [errors (vec (:errors checked))
             first-error (or (first (remove #(#{"allOf" "anyOf" "oneOf"} (:keyword %)) errors))
                             (first errors))]
         (throw (ex-info (str "invalid JSON Schema at "
                              (or (not-empty (:instanceLocation first-error)) "/")
                              ": " (:error first-error))
                         {:skjema/error :schema/invalid :errors errors}))))
     (assoc compiled :fast-validator (::compiled (meta schema))))))

(defn compiled-schema? [x] (boolean (:skjema/compiled x)))

(defn- compiled
  "The schema as compiled, compiling it first when a caller passed a raw one."
  [schema opts]
  (if (compiled-schema? schema) schema (compile-schema schema opts)))

(defn validator
  "The verdict as a bare function of the instance. It closes over the compiled
   predicate, so a call reaches it without a map lookup - take this when the same
   schema validates more than once. Evaluation stops at the first keyword that
   refuses and no error is built on the way."
  ([schema] (validator schema nil))
  ([schema opts]
   (let [c (compiled schema opts)]
     (if-let [^Predicate fast (:fast-validator c)]
       (fn [instance] (.test fast instance))
       (let [ctx (:quiet-ctx c)
             schema (:schema c)]
         (fn [instance] (true? (:valid? (eval-schema ctx schema instance)))))))))

(defn explainer
  "The reasons as a bare function of the instance: nil when it validates, else
   JSON Schema BASIC output, at most five errors of it. The counterpart of
   `validator` for the same schema."
  ([schema] (explainer schema nil))
  ([schema opts]
   (let [c (compiled schema opts)
         ctx (:ctx c)
         schema (:schema c)]
     (if-let [^Predicate fast (:fast-validator c)]
        ;; A passing instance is the common case and costs no error machinery: the
        ;; compiled predicate answers it, and only an instance that has something
        ;; to explain pays for the errors it has.
       (if-let [explain-fast (:fast-explainer c)]
         explain-fast
         (fn [instance]
           (when-not (.test fast instance)
             (let [r (eval-schema ctx schema instance true)]
               (when-not (:valid? r)
                 (report (:errors r)))))))
       (fn [instance]
         (let [r (eval-schema ctx schema instance)]
           (when-not (:valid? r)
             (report (:errors r)))))))))

(defn validate
  "True when `instance` satisfies the schema. `validator` is the same answer
   without the per-call dispatch."
  ([schema instance] (validate schema instance nil))
  ([schema instance opts] ((validator schema opts) instance)))

(defn explain
  "Nil when `instance` satisfies the schema, otherwise JSON Schema BASIC output -
   `{:valid false :errors [...]}` - where every error keeps its instance and
   keyword locations and adds `:keyword` plus keyword-specific `:params`, so a
   caller can render or act on it without parsing the human `:error` string. The
   entire answer is a JSON value, and it carries the first five errors of the
   instance: a reader acts on those, and the rest only cost the walk that finds
   them."
  ([schema instance] (explain schema instance nil))
  ([schema instance opts] ((explainer schema opts) instance)))
