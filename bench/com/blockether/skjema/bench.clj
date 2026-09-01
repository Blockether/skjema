(ns com.blockether.skjema.bench
  "How fast the validator is, measured against malli on the same data.

   Both sides are COMPILED before timing - `skjema/compile-schema` against
   `m/validator`/`m/explainer` - and handed the same Clojure data, so a row
   measures validation. Compilation itself is one row, because a caller pays
   it once.

   `clojure -M:bench`, or `clojure -M:bench <case>` for one row."
  (:require [clojure.string :as str]
            [criterium.core :as cc]
            [malli.core :as m]
            [com.blockether.skjema.core :as skjema]))

;; --- the schemas, once in each language -------------------------------------

(def user-json-schema
  {"$schema" "https://json-schema.org/draft/2020-12/schema"
   "type" "object"
   "required" ["id" "name" "email" "age" "tags" "address"]
   "properties"
   {"id" {"type" "string" "pattern" "^[0-9a-f]{8}$"}
    "name" {"type" "string" "minLength" 1 "maxLength" 64}
    "email" {"type" "string" "minLength" 3 "maxLength" 254}
    "age" {"type" "integer" "minimum" 0 "maximum" 130}
    "active" {"type" "boolean"}
    "score" {"type" "number" "minimum" 0 "maximum" 1}
    "role" {"enum" ["admin" "user" "guest"]}
    "tags" {"type" "array" "minItems" 1 "maxItems" 10
            "items" {"type" "string" "minLength" 1}}
    "address" {"type" "object"
               "required" ["city" "zip"]
               "properties" {"city" {"type" "string" "minLength" 1}
                             "zip" {"type" "string" "pattern" "^[0-9]{5}$"}
                             "country" {"type" "string" "minLength" 2 "maxLength" 2}}}}})

(def user-malli-schema
  [:map
   ["id" [:and :string [:re #"^[0-9a-f]{8}$"]]]
   ["name" [:string {:min 1 :max 64}]]
   ["email" [:string {:min 3 :max 254}]]
   ["age" [:int {:min 0 :max 130}]]
   ["active" {:optional true} :boolean]
   ["score" {:optional true} [:double {:min 0 :max 1}]]
   ["role" {:optional true} [:enum "admin" "user" "guest"]]
   ["tags" [:vector {:min 1 :max 10} [:string {:min 1}]]]
   ["address" [:map
               ["city" [:string {:min 1}]]
               ["zip" [:and :string [:re #"^[0-9]{5}$"]]]
               ["country" {:optional true} [:string {:min 2 :max 2}]]]]])

(def user
  {"id" "0a1b2c3d"
   "name" "Ada Lovelace"
   "email" "ada@example.com"
   "age" 36
   "active" true
   "score" 0.75
   "role" "admin"
   "tags" ["mathematics" "engines"]
   "address" {"city" "London" "zip" "12345" "country" "GB"}})

(def bad-user (assoc user "age" 500 "tags" []))

(def scalar-json-schema {"type" "string" "minLength" 3 "maxLength" 32})
(def scalar-malli-schema [:string {:min 3 :max 32}])

(def numbers-json-schema
  {"type" "array" "items" {"type" "integer" "minimum" 0 "maximum" 1000}})
(def numbers-malli-schema [:vector [:int {:min 0 :max 1000}]])
(def numbers (vec (range 1000)))

;; --- measurement ------------------------------------------------------------

(defn- mean-ns ^double [f]
  (let [{:keys [mean]} (cc/quick-benchmark (f) {})]
    (* 1e9 (double (first mean)))))

(defn- fmt [^double ns]
  (cond (< ns 1e3) (format "%.0f ns" ns)
        (< ns 1e6) (format "%.2f us" (/ ns 1e3))
        :else (format "%.2f ms" (/ ns 1e6))))

(def ^:private cases
  [{:id "user-valid"
    :what "object of 9 members, nested object, array of 2 - VALID"
    :skjema (let [v (skjema/validator user-json-schema)] #(v user))
    :malli (let [v (m/validator user-malli-schema)] #(v user))
    :expect true}
   {:id "user-invalid"
    :what "the same object, two members wrong - INVALID"
    :skjema (let [v (skjema/validator user-json-schema)] #(v bad-user))
    :malli (let [v (m/validator user-malli-schema)] #(v bad-user))
    :expect false}
   {:id "user-errors"
    :what "the same object, ERRORS reported (BASIC output vs malli explainer)"
    :skjema (let [e (skjema/explainer user-json-schema)] #(e bad-user))
    :malli (let [e (m/explainer user-malli-schema)] #(e bad-user))
    :expect nil}
   {:id "explain-valid"
    :what "the same object VALID, errors asked for anyway - the common case"
    :skjema (let [e (skjema/explainer user-json-schema)] #(e user))
    :malli (let [e (m/explainer user-malli-schema)] #(e user))
    :expect nil}
   {:id "scalar"
    :what "one string, minLength/maxLength - per-call overhead"
    :skjema (let [v (skjema/validator scalar-json-schema)] #(v "hello"))
    :malli (let [v (m/validator scalar-malli-schema)] #(v "hello"))
    :expect true}
   {:id "numbers-1000"
    :what "array of 1000 integers with bounds - throughput"
    :skjema (let [v (skjema/validator numbers-json-schema)] #(v numbers))
    :malli (let [v (m/validator numbers-malli-schema)] #(v numbers))
    :expect true}
   {:id "prepare"
    :what "preparing the validator itself, paid once"
    :skjema #(skjema/validator user-json-schema)
    :malli #(m/validator user-malli-schema)
    :expect nil}])

(defn- check! [{:keys [id skjema malli expect]}]
  (when (some? expect)
    (let [s (skjema) m (malli)]
      (when-not (and (= expect s) (= expect m))
        (throw (ex-info (str "case " id " does not measure the same verdict")
                        {:skjema s :malli m :expected expect}))))))

(defn -main [& args]
  (let [wanted (set args)
        rows (cond->> cases (seq wanted) (filter (comp wanted :id)))]
    (run! check! rows)
    (println)
    (println (format "%-14s %12s %12s %10s   %s" "case" "skjema" "malli" "ratio" "what"))
    (println (str/join (repeat 110 "-")))
    (doseq [{:keys [id what] :as row} rows
            :let [s (mean-ns (:skjema row))
                  m (mean-ns (:malli row))]]
      (println (format "%-14s %12s %12s %9.2fx   %s" id (fmt s) (fmt m) (/ s m) what)))
    (println)
    (println "ratio > 1 means skjema is that many times SLOWER than malli.")
    (flush)
    (shutdown-agents)))
