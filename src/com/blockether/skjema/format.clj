(ns com.blockether.skjema.format
  "What every `format` the 2020-12 specification names actually asserts.

   `format` is an ANNOTATION by default and this namespace is only consulted
   when a schema asks for the assertion - by declaring the format-assertion
   vocabulary, or because the caller compiled with `:format-assertion true`.
   An unknown format is not an error: it annotates and asserts nothing, which
   is why `known?` is a question worth asking before `valid?` answers one.

   Every check is the grammar of the specification that owns it, written out
   rather than delegated: `java.net.URI` is RFC 2396 and accepts what RFC 3986
   refuses, `InetAddress` resolves names, and `DateTimeFormatter` has no
   opinion about a leap second, which RFC 3339 permits at exactly one instant
   of the day. A grammar that is a walk over the characters is walked in
   `Formats.java`, because a format is asserted once per instance and a
   regular expression that allocates its groups is the wrong tool there. What
   is left here is what composes those: an address is a local part and a host,
   and a host name is the ASCII half of an internationalized one."
  (:require [clojure.string :as str]
            [com.blockether.skjema.idn :as idn]
            [com.blockether.skjema.regex :as regex])
  (:import (com.blockether.skjema Formats)))

;; Hosts and addresses

(defn- hostname?
  "RFC 1123, and an `xn--` label still has to be Punycode that decodes: a host
   name is the ASCII half of an internationalized one, not a looser grammar."
  [^String s]
  (and (Formats/ascii s)
       (idn/hostname? s)))

(defn- address-literal?
  "A domain given as an address rather than a name: `[127.0.0.1]` or
   `[IPv6:::1]`."
  [^String domain]
  (and (str/starts-with? domain "[")
       (str/ends-with? domain "]")
       (let [body (subs domain 1 (dec (count domain)))]
         (if (str/starts-with? (str/lower-case body) "ipv6:")
           (Formats/ipv6 (subs body 5))
           (Formats/ipv4 body)))))

(defn- email? [^String s]
  (let [at (.lastIndexOf s "@")]
    (and (pos? at)
         (Formats/mailLocal (subs s 0 at) false)
         (let [domain (subs s (inc at))]
           (or (hostname? domain) (address-literal? domain))))))

(defn- idn-email? [^String s]
  (let [at (.lastIndexOf s "@")]
    (and (pos? at)
         (Formats/mailLocal (subs s 0 at) true)
         (let [domain (subs s (inc at))]
           (or (idn/hostname? domain) (address-literal? domain))))))

(def ^:private checks
  "Every format 2020-12 defines, and the question each one asks."
  {"date" #(Formats/date %)
   "date-time" #(Formats/dateTime %)
   "time" #(Formats/time %)
   "duration" #(Formats/duration %)
   "email" email?
   "idn-email" idn-email?
   "hostname" hostname?
   "idn-hostname" idn/hostname?
   "ipv4" #(Formats/ipv4 %)
   "ipv6" #(Formats/ipv6 %)
   "uri" #(Formats/uri %)
   "uri-reference" #(Formats/uriReference %)
   "iri" #(Formats/iri %)
   "iri-reference" #(Formats/iriReference %)
   "uri-template" #(Formats/uriTemplate %)
   "uuid" #(Formats/uuid %)
   "json-pointer" #(Formats/jsonPointer %)
   "relative-json-pointer" #(Formats/relativeJsonPointer %)
   "regex" regex/ecma?})

(defn known?
  "Whether `format` is one this library asserts. An unknown format is a plain
   annotation, which the specification requires and callers rely on."
  [format]
  (contains? checks format))

(defn valid?
  "Whether `s` satisfies `format`. An unknown format is satisfied by every
   string, and so is every instance that is not a string at all."
  [format s]
  (if-let [check (checks format)]
    (or (not (string? s))
        (try (boolean (check s))
             (catch Exception _ false)))
    true))
