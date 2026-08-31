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
   of the day."
  (:require [clojure.string :as str]
            [com.blockether.skjema.idn :as idn]
            [com.blockether.skjema.regex :as regex])
  (:import (java.time LocalDate)))

;; Dates, times and durations (RFC 3339)

(def ^:private date-pattern #"(\d{4})-(\d{2})-(\d{2})")

(defn- date?
  [^String s]
  (boolean
   (when-let [[_ y m d] (re-matches date-pattern s)]
     (try (LocalDate/of (parse-long y) (parse-long m) (parse-long d)) true
          (catch Exception _ false)))))

(def ^:private time-pattern
  #"(\d{2}):(\d{2}):(\d{2})(?:\.\d+)?(?:[Zz]|([+-])(\d{2}):(\d{2}))")

(defn- time?
  "RFC 3339 full-time: the offset is not optional, and the leap second is only
   a second of the day if it is the LAST one - 23:59:60 in UTC, whatever the
   offset spells it as locally."
  [^String s]
  (boolean
   (when-let [[_ h m sec sign oh om] (re-matches time-pattern s)]
     (let [h (long (parse-long h)) m (long (parse-long m)) sec (long (parse-long sec))
           oh (some-> oh parse-long) om (some-> om parse-long)
           offset (long (if sign (* (if (= sign "-") -1 1) (+ (* 60 (long oh)) (long om))) 0))]
       (and (<= h 23) (<= m 59)
            (or (nil? oh) (and (<= (long oh) 23) (<= (long om) 59)))
            (if (= sec 60)
              (= 1439 (mod (- (+ (* 60 h) m) offset) 1440))
              (<= sec 59)))))))

(defn- date-time?
  [^String s]
  (and (> (count s) 11)
       (contains? #{\T \t} (.charAt s 10))
       (date? (subs s 0 10))
       (time? (subs s 11))))

(def ^:private duration-pattern
  #"P(?:\d+W|(?:\d+Y(?:\d+M(?:\d+D)?)?|\d+M(?:\d+D)?|\d+D)(?:T(?:\d+H(?:\d+M(?:\d+S)?)?|\d+M(?:\d+S)?|\d+S))?|T(?:\d+H(?:\d+M(?:\d+S)?)?|\d+M(?:\d+S)?|\d+S))")

(defn- duration? [^String s] (boolean (re-matches duration-pattern s)))

;; Hosts and addresses

(defn- hostname?
  "RFC 1123, and an `xn--` label still has to be Punycode that decodes: a host
   name is the ASCII half of an internationalized one, not a looser grammar."
  [^String s]
  (and (every? #(< (int %) 128) s)
       (idn/hostname? s)))

(def ^:private ipv4-pattern
  #"(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])(?:\.(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])){3}")

(defn- ipv4? [^String s] (boolean (re-matches ipv4-pattern s)))

(def ^:private ipv6-pattern
  (let [h16 "[0-9A-Fa-f]{1,4}"
        v4 "(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])(?:\\.(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])){3}"
        ls32 (str "(?:" h16 ":" h16 "|" v4 ")")]
    (re-pattern
     (str "(?:"
          "(?:" h16 ":){6}" ls32
          "|::(?:" h16 ":){5}" ls32
          "|(?:" h16 ")?::(?:" h16 ":){4}" ls32
          "|(?:(?:" h16 ":){0,1}" h16 ")?::(?:" h16 ":){3}" ls32
          "|(?:(?:" h16 ":){0,2}" h16 ")?::(?:" h16 ":){2}" ls32
          "|(?:(?:" h16 ":){0,3}" h16 ")?::" h16 ":" ls32
          "|(?:(?:" h16 ":){0,4}" h16 ")?::" ls32
          "|(?:(?:" h16 ":){0,5}" h16 ")?::" h16
          "|(?:(?:" h16 ":){0,6}" h16 ")?::"
          ")"))))

(defn- ipv6? [^String s] (boolean (re-matches ipv6-pattern s)))

;; Identifiers (RFC 3986 and RFC 3987)

(def ^:private ucschar
  "\\xA0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF\\x{10000}-\\x{1FFFD}\\x{20000}-\\x{2FFFD}\\x{30000}-\\x{3FFFD}\\x{40000}-\\x{4FFFD}\\x{50000}-\\x{5FFFD}\\x{60000}-\\x{6FFFD}\\x{70000}-\\x{7FFFD}\\x{80000}-\\x{8FFFD}\\x{90000}-\\x{9FFFD}\\x{A0000}-\\x{AFFFD}\\x{B0000}-\\x{BFFFD}\\x{C0000}-\\x{CFFFD}\\x{D0000}-\\x{DFFFD}\\x{E1000}-\\x{EFFFD}")

(def ^:private iprivate
  "\\x{E000}-\\x{F8FF}\\x{F0000}-\\x{FFFFD}\\x{100000}-\\x{10FFFD}")

(defn- identifier-patterns
  "The RFC 3986 grammar, or RFC 3987's when `iri?` - the two differ only in
   which characters an unreserved one may be."
  [iri?]
  (let [unreserved (str "A-Za-z0-9\\-._~" (when iri? ucschar))
        sub "!$&'()*+,;="
        pct "%[0-9A-Fa-f]{2}"
        pchar (str "(?:[" unreserved sub ":@]|" pct ")")
        query (str "(?:[" unreserved sub ":@/?" (when iri? iprivate) "]|" pct ")*")
        fragment (str "(?:[" unreserved sub ":@/?]|" pct ")*")
        v4 "(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])(?:\\.(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])){3}"
        ipvfuture (str "[vV][0-9A-Fa-f]+\\.[" unreserved sub ":]+")
        host (str "(?:\\[(?:" ipv6-pattern "|" ipvfuture ")\\]|" v4 "|(?:[" unreserved sub "]|" pct ")*)")
        userinfo (str "(?:[" unreserved sub ":]|" pct ")*")
        authority (str "(?:" userinfo "@)?" host "(?::[0-9]*)?")
        segment (str pchar "*")
        abempty (str "(?:/" segment ")*")
        absolute (str "/(?:" pchar "+(?:/" segment ")*)?")
        rootless (str pchar "+(?:/" segment ")*")
        noscheme (str "(?:[" unreserved sub "@]|" pct ")+(?:/" segment ")*")
        tail (str "(?:\\?" query ")?(?:#" fragment ")?")
        uri (str "[A-Za-z][A-Za-z0-9+\\-.]*:(?://" authority abempty "|" absolute "|" rootless "|)" tail)
        relative (str "(?://" authority abempty "|" absolute "|" noscheme "|)" tail)]
    {:absolute (re-pattern uri)
     :reference (re-pattern (str "(?:" uri "|" relative ")"))}))

(def ^:private uri-forms (delay (identifier-patterns false)))
(def ^:private iri-forms (delay (identifier-patterns true)))

(def ^:private uri-template-pattern
  (delay
    (let [pct "%[0-9A-Fa-f]{2}"
          literal (str "(?:[\\x21\\x23-\\x24\\x26-\\x3B\\x3D\\x3F-\\x5B\\x5D\\x5F\\x61-\\x7A\\x7E"
                       ucschar iprivate "]|" pct ")")
          varchar (str "(?:[A-Za-z0-9_]|" pct ")")
          varname (str varchar "(?:\\.?" varchar ")*")
          varspec (str varname "(?::[1-9][0-9]{0,3}|\\*)?")
          expression (str "\\{[+#./;?&=,!@|]?" varspec "(?:," varspec ")*\\}")]
      (re-pattern (str "(?:" literal "|" expression ")*")))))

;; Mail addresses (RFC 5321 and RFC 6531)

(def ^:private atext "A-Za-z0-9!#$%&'*+/=?^_`{|}~\\-")

(def ^:private non-ascii
  "RFC 6531 widens atext to every character above ASCII, and not only to the
   ones a domain name may hold."
  "\\x80-\\uD7FF\\uE000-\\x{10FFFF}")

(defn- local-part-pattern [iri?]
  (let [atext (str atext (when iri? non-ascii))]
    (re-pattern (str "(?:[" atext "]+(?:\\.[" atext "]+)*"
                     "|\"(?:[\\x20-\\x21\\x23-\\x5B\\x5D-\\x7E" (when iri? non-ascii)
                     "]|\\\\[\\x20-\\x7E])*\")"))))

(def ^:private mail-local (delay (local-part-pattern false)))
(def ^:private idn-mail-local (delay (local-part-pattern true)))

(defn- address-literal?
  "A domain given as an address rather than a name: `[127.0.0.1]` or
   `[IPv6:::1]`."
  [^String domain]
  (and (str/starts-with? domain "[")
       (str/ends-with? domain "]")
       (let [body (subs domain 1 (dec (count domain)))]
         (if (str/starts-with? (str/lower-case body) "ipv6:")
           (ipv6? (subs body 5))
           (ipv4? body)))))

(defn- email? [^String s]
  (let [at (.lastIndexOf s "@")]
    (and (pos? at)
         (boolean (re-matches @mail-local (subs s 0 at)))
         (let [domain (subs s (inc at))]
           (or (hostname? domain) (address-literal? domain))))))

(defn- idn-email? [^String s]
  (let [at (.lastIndexOf s "@")]
    (and (pos? at)
         (boolean (re-matches @idn-mail-local (subs s 0 at)))
         (let [domain (subs s (inc at))]
           (or (idn/hostname? domain) (address-literal? domain))))))

;; Pointers (RFC 6901)

(def ^:private json-pointer-pattern #"(?:/(?:[^~/]|~[01])*)*")

(def ^:private relative-json-pointer-pattern
  #"(?:0|[1-9][0-9]*)(?:#|(?:/(?:[^~/]|~[01])*)*)")

(def ^:private uuid-pattern
  #"[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}")

(def ^:private checks
  "Every format 2020-12 defines, and the question each one asks."
  {"date" date?
   "date-time" date-time?
   "time" time?
   "duration" duration?
   "email" email?
   "idn-email" idn-email?
   "hostname" hostname?
   "idn-hostname" idn/hostname?
   "ipv4" ipv4?
   "ipv6" ipv6?
   "uri" #(boolean (re-matches (:absolute @uri-forms) %))
   "uri-reference" #(boolean (re-matches (:reference @uri-forms) %))
   "iri" #(boolean (re-matches (:absolute @iri-forms) %))
   "iri-reference" #(boolean (re-matches (:reference @iri-forms) %))
   "uri-template" #(boolean (re-matches @uri-template-pattern %))
   "uuid" #(boolean (re-matches uuid-pattern %))
   "json-pointer" #(boolean (re-matches json-pointer-pattern %))
   "relative-json-pointer" #(boolean (re-matches relative-json-pointer-pattern %))
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
