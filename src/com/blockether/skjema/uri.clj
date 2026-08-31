(ns com.blockether.skjema.uri
  "URI reference resolution and JSON Pointer tokens - the addressing half of
   JSON Schema.

   Identifiers in a schema are URI REFERENCES: `$id` moves the base, `$ref`
   resolves against whatever base encloses it, and a fragment is either an
   anchor name or a JSON Pointer. Everything here is string work; nothing
   fetches anything, because a validator that resolves a reference over the
   network is a validator that fails differently on every machine."
  (:require [clojure.string :as str])
  (:import (java.io ByteArrayOutputStream)
           (java.net URI)))

(defn strip-fragment
  "The URI without its fragment. `nil` in, empty string out, because the base of
   a schema that never declared one is the empty URI."
  ^String [^String u]
  (let [u (or u "")
        i (.indexOf u "#")]
    (if (neg? i) u (subs u 0 i))))

(defn fragment
  "The fragment of `u` WITHOUT its `#`, or nil when it carries none. An empty
   fragment (`uri#`) answers the empty string, which is a different fact."
  [^String u]
  (let [i (.indexOf (or u "") "#")]
    (when-not (neg? i) (subs u (inc i)))))

(def ^:private scheme-pattern #"^[A-Za-z][A-Za-z0-9+.\-]*:")

(defn absolute?
  "True when the reference carries a scheme, so it resolves to itself."
  [^String u]
  (boolean (and u (re-find scheme-pattern u))))

(defn resolve-ref
  "Resolve reference `ref` against `base`, RFC 3986 style.

   The two cases a URI library gets wrong for schemas are handled first: an
   empty reference is the base itself, and a fragment-only reference replaces
   the base's fragment - including against an opaque base like `urn:uuid:...`,
   where `java.net.URI/resolve` would hand the fragment straight back."
  ^String [^String base ^String ref]
  (let [base (or base "")]
    (cond
      (or (nil? ref) (= "" ref)) base
      (str/starts-with? ref "#") (str (strip-fragment base) ref)
      (absolute? ref) ref
      (= "" base) ref
      (absolute? base) (try
                         (str (.resolve (URI. base) ref))
                         (catch Exception _ ref))
      :else ref)))

(defn percent-decode
  "Decode `%XX` escapes as UTF-8. A pointer fragment is part of a URI, so
   `/foo%20bar` and `/foo bar` name the same member."
  ^String [^String s]
  (if-not (str/includes? s "%")
    s
    (let [out (ByteArrayOutputStream.)
          n (.length s)]
      (loop [i 0]
        (when (< i n)
          (let [c (.charAt s i)]
            (if (and (= c \%) (<= (+ i 3) n))
              (do (.write out (int (Integer/parseInt (subs s (inc i) (+ i 3)) 16)))
                  (recur (+ i 3)))
              (let [^bytes b (.getBytes (String/valueOf c) "UTF-8")]
                (.write out b 0 (alength b))
                (recur (inc i)))))))
      (String. (.toByteArray out) "UTF-8"))))

(defn unescape-token
  "One JSON Pointer token as the member name it addresses: `~1` is `/` and `~0`
   is `~`, in that order, so `~01` decodes to `~1` and not to `/`."
  ^String [^String t]
  (-> (percent-decode t)
      (str/replace "~1" "/")
      (str/replace "~0" "~")))

(defn escape-token
  "One member name as a JSON Pointer token."
  ^String [^String t]
  (-> t
      (str/replace "~" "~0")
      (str/replace "/" "~1")))

(defn pointer-tokens
  "Split a JSON Pointer into decoded tokens. The empty pointer addresses the
   document itself and answers no tokens."
  [^String pointer]
  (if (or (nil? pointer) (= "" pointer))
    []
    (mapv unescape-token (rest (str/split pointer #"/" -1)))))
