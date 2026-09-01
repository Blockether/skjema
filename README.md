# skjema

JSON Schema 2020-12 validation for Clojure.

```clojure
com.blockether/skjema {:mvn/version "0.3.0"}
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
(def valid? (skjema/validator schema))
(def why (skjema/explainer schema))

(valid? {"name" "Ada"})   ;; => true
(why {"name" "Ada"})      ;; => nil
```

`read-schema` takes JSON text, a `Path`, a `File`, a URL or classpath resource,
an `InputStream` or a `Reader`. `write-schema` returns compact JSON text.

References resolve from an explicit registry; nothing is fetched over the
network:

```clojure
(skjema/compile-schema schema
  {:base "https://example.com/user.json"
   :registry {"https://example.com/tag.json" tag-schema}
   :format-assertion true})
```

## Errors

`explain` answers nil, or BASIC output locations extended with `keyword`,
keyword-specific `params` and a readable `error`. Reading, compiling and writing
throw `ExceptionInfo` carrying a `:skjema/error` category and the original
cause.

## Verify

```bash
clojure -M:test    # official JSON-Schema-Test-Suite, required and optional 2020-12
clojure -M:bench   # against malli, schema compiled on both sides
```

## License

MIT — see [LICENSE](LICENSE) and [NOTICE](NOTICE).
