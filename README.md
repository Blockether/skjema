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
- **Validation works on parsed data.** `compile` turns a schema into a closure
  once; `validate` walks the instance, not the schema map. The instance may come
  from `skjema.json`, from charred, from anywhere — it is plain Clojure data.
- **Errors and annotations are the specification's own output format** —
  `valid`, `instanceLocation`, `keywordLocation`, `errors` — as a Clojure map, so
  writing it back out is one call and no second vocabulary exists.

## Status

The reader is done and its gate is green: **318/318** files of
[JSONTestSuite](https://github.com/nst/JSONTestSuite) (`y_` accepted, `n_`
refused, `i_` answered), zero reflection warnings, zero dependencies. The
validator itself is next. The gate is the official
[JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
for draft 2020-12, `optional/` excluded, and
[JSONTestSuite](https://github.com/nst/JSONTestSuite) for the reader. Until both
are green this is not a JSON Schema validator, only a predicate with JSON
syntax.

## License

MIT — see [LICENSE](LICENSE) and [NOTICE](NOTICE).
