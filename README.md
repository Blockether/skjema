# skjema

JSON Schema 2020-12 validation for Clojure.

```clojure
com.blockether/skjema {:mvn/version "0.3.1"}
```

## Use

```clojure
(require '[com.blockether.skjema.core :as skjema]
         '[clojure.java.io :as io])

(def schema
  (skjema/read-schema (io/resource "user.schema.json")))

(skjema/validate schema {"name" "Ada"})
;; => true

(skjema/explain schema {"name" 42})
;; => {:valid false
;;     :errors [{:instanceLocation "/name"
;;               :keywordLocation "/properties/name/type"
;;               :keyword "type"
;;               :params {:type "string"}
;;               :error "expected string, got integer"}]}
```

Compile once and keep the function when the same schema validates repeatedly -
that is the fast path, and it is what the benchmark measures:

```clojure
(def compiled (skjema/compile-schema schema))
(def valid?   (skjema/validator compiled))
(def why      (skjema/explainer compiled))

(valid? {"name" "Ada"})   ;; => true
(why {"name" "Ada"})      ;; => nil
```

## API

Seven calls in `com.blockether.skjema.core`, and that is the whole surface.
Instances and schemas are plain data with JSON string keys.

| call | answers |
| --- | --- |
| `(read-schema source)` | the parsed JSON. `source` is JSON text, a `Path`, a `File`, a URL or classpath resource, an `InputStream` or a `Reader`. |
| `(compile-schema schema)`<br>`(compile-schema schema opts)` | the schema indexed and prepared once: references resolved, the document meta-validated, a compiled predicate attached where one can be built. |
| `(compiled-schema? x)` | true when `x` came out of `compile-schema`. |
| `(validator schema)`<br>`(validator schema opts)` | `(fn [instance] -> true/false)`, closed over the compiled predicate. |
| `(explainer schema)`<br>`(explainer schema opts)` | `(fn [instance] -> nil, or BASIC output)`. |
| `(validate schema instance)`<br>`(validate schema instance opts)` | the verdict for one instance. |
| `(explain schema instance)`<br>`(explain schema instance opts)` | nil, or the reasons for one instance. |

`schema` is a raw schema or a compiled one; passing a raw schema compiles it on
the spot, so hold the compiled value when the same schema is used twice.

`opts` are the compile options, and only `compile-schema` reads them:

```clojure
(skjema/compile-schema schema
  {:base "https://example.com/user.json"
   :registry {"https://example.com/tag.json" tag-schema}
   :format-assertion true})
```

References resolve from that registry alone; nothing is fetched over the
network. `:format-assertion` turns `format` from an annotation into an
assertion.

## Verify

```bash
clojure -T:build compile-java   # the Java sources, once per checkout
clojure -M:test                 # official JSON-Schema-Test-Suite, required and optional 2020-12
clojure -M:bench                # against malli, schema compiled on both sides
```

## License

MIT - see [LICENSE](LICENSE) and [NOTICE](NOTICE).
