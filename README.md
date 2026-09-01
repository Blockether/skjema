# skjema

Fast JSON Schema 2020-12 validation for Clojure. `skjema` is Norwegian for
*schema*.

```clojure
com.blockether/skjema {:mvn/version "0.2.0"}
```

## Use

```clojure
(require '[com.blockether.skjema.core :as skjema])

(def schema
  (skjema/read-schema (java.nio.file.Path/of "user.schema.json" (make-array String 0))))

(def validator (skjema/compile-schema schema))

(skjema/valid? validator {"name" "Ada"})
;; => true

(skjema/validate validator {"name" 42})
;; => {:valid false
;;     :errors [{:instanceLocation "/name"
;;               :keywordLocation "/properties/name/type"
;;               :keyword "type"
;;               :params {:type "string"}
;;               :error "expected string, got integer"}]}
```

`read-schema` accepts JSON text, `Path`, `File`, URL/classpath resource,
`InputStream`, or `Reader`. `write-schema` returns compact JSON text. Both use
[charred](https://github.com/cnuernber/charred).

Compile once when a schema is reused. References are resolved from an explicit
registry; skjema never fetches them:

```clojure
(def validator
  (skjema/compile-schema schema
    {:base "https://example.com/user.json"
     :registry {"https://example.com/tag.json" tag-schema}
     :format-assertion true}))
```

## Errors

`validate` returns JSON Schema BASIC output locations and adds `keyword`,
keyword-specific `params`, and a human-readable `error`. A successful result is
`{:valid true}`. Parsing, compilation, and writing throw `ExceptionInfo` with a
`:skjema/error` category and preserve the original cause.

## Verification

The suite covers all required and optional draft 2020-12 cases from the official
JSON-Schema-Test-Suite. Java owns the compiled validation and IDN hot loops;
Clojure owns complete evaluation and structured errors.

```bash
clojure -M:test
clojure -M:bench
```

The benchmark prepares both schemas before timing validation. Run it on the
machine and JVM that matter to your workload.

## License

MIT — see [LICENSE](LICENSE) and [NOTICE](NOTICE).
