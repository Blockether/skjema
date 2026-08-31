# skjema

**JSON Schema 2020-12 validation for Clojure.** No dependencies, no reflection,
native-image safe. `skjema` is Norwegian for *schema*.

```clojure
com.blockether/skjema {:mvn/version "0.1.0"}
```

## Why another validator

Every JVM validator worth using binds Jackson `databind`, which means reflection
and a reachability-metadata chase in a GraalVM native image, plus a JSON stack a
consumer did not choose. `skjema` binds nothing:

- **Schemas are JSON.** Written as `.json`, copied straight from the
  specification or the test suite, readable by every other tool that speaks
  JSON Schema. No EDN dialect, no translation layer, no drift test between the
  two spellings.
- **Validation works on parsed data.** `compile` indexes a schema once;
  `validate` walks the instance, not the schema map. The instance may come
  from `skjema.json`, from charred, from anywhere — it is plain Clojure data.
- **Errors are the specification's own output format** — `valid`,
  `instanceLocation`, `keywordLocation`, `absoluteKeywordLocation`, `error` —
  as a Clojure map, so writing it back out is one call and no second
  vocabulary exists.

## Use

```clojure
(require '[com.blockether.skjema.core :as skjema]
         '[com.blockether.skjema.json :as json])

(def schema (json/read-str (slurp "user.schema.json")))

(skjema/valid? schema {"name" "Ada"})
;; => true

(skjema/validate schema {"name" 42})
;; => {:valid false
;;     :errors [{:instanceLocation "/name"
;;               :keywordLocation "/properties/name/type"
;;               :absoluteKeywordLocation "https://example.com/user.json#/properties/name/type"
;;               :error "expected string, got integer"}]}

(json/write-str (skjema/validate schema {"name" 42}))
;; => the same answer as the specification's BASIC output, key for key
```

`compile` once when the schema is reused, and hand it whatever it references —
nothing is ever fetched:

```clojure
(def compiled (skjema/compile schema {:base "https://example.com/user.json"
                                      :registry {"https://example.com/tag.json" tag-schema}}))
(skjema/valid? compiled instance)
```

`format` annotates rather than asserts, which is what the specification says by
default. Ask for the assertion — every format 2020-12 names is implemented,
`date-time` through `idn-hostname` — with an option, or by declaring the
format-assertion vocabulary in a meta-schema:

```clojure
(skjema/valid? {"format" "idn-hostname"} "\u5b9f\u4f8b.\u30c6\u30b9\u30c8" {:format-assertion true})
```

## Status

Green, and the gate says what that means:

- **1299 assertions** of the official
  [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
  for draft 2020-12, every required file.
- **1012 assertions** of that suite's `optional/` files — the formats, the
  ECMAScript regular expressions, arbitrary-precision numbers, the draft-07
  `dependencies`, and a reference that crosses into an older draft.
- **318/318** files of [JSONTestSuite](https://github.com/nst/JSONTestSuite)
  for the reader (`y_` accepted, `n_` refused, `i_` answered).
- Zero reflection warnings, zero dependencies.

Two things the suite does not ask for and this library does not do: it never
fetches a document over the network, and it evaluates the 2020-12 dialect —
a resource that declares 2019-09, draft-07, draft-06 or draft-04 is read with
the keywords THAT draft defines, so `$recursiveRef` is recognized without being
followed.

## License

MIT — see [LICENSE](LICENSE) and [NOTICE](NOTICE).
