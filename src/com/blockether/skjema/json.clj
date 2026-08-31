(ns com.blockether.skjema.json
  "JSON reader and writer - RFC 8259, no dependencies, no reflection.

   `read-str` / `read-bytes` answer plain Clojure data: an object is a map with
   STRING keys, an array a vector, a number a Long / BigInteger / Double, a
   string a String, `true` / `false` a Boolean and `null` nil. That is the only
   shape skjema validates: JSON Schema property names ARE strings, so keeping
   the keys strings means a schema and its instance are compared without a
   second spelling and without a round trip.

   Reading is TOTAL: every rejection is an `ex-info` carrying
   `:skjema/error :json/parse` with `:offset`, `:line` and `:column`, never a
   partial value and never a raw Java exception. Depth is BOUNDED
   (`:max-depth`, default 1024) because recursive descent over hostile input is
   otherwise a StackOverflowError - an Error is not something a caller can
   handle, so a document that is too deep is REJECTED like any other malformed
   input.

   This reader exists so the library depends on nothing. It is deliberately
   plain: schemas are read once, at compile time, and an instance normally
   arrives already parsed by whatever the host uses."
  (:import (java.nio ByteBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets)))

(def ^:private default-max-depth
  "How deep a document may nest before it is rejected. Above the 500 levels the
   conformance suite calls implementation-defined, below anything that threatens
   the JVM stack."
  1024)

(defn- fail!
  "Reject the document. `i` is the character OFFSET the reader stopped at; the
   line/column are derived from it so the caller can point at the byte without
   the reader tracking two counters through every loop."
  [^String s ^long i ^String msg]
  (let [n (min i (.length s))
        line (loop [k 0 line 1] (if (>= k n)
                                  line
                                  (recur (inc k) (if (= \newline (.charAt s k)) (inc line) line))))
        bol (loop [k (dec n)] (cond (neg? k) 0
                                    (= \newline (.charAt s k)) (inc k)
                                    :else (recur (dec k))))]
    (throw (ex-info (str "JSON parse error at line " line ", column " (inc (- n bol)) ": " msg)
                    {:skjema/error :json/parse
                     :offset i
                     :line line
                     :column (inc (- n bol))
                     :reason msg}))))

(defn- hex-digit
  "The value of one `\\uXXXX` hex digit, or nil. `Integer/parseInt` is not usable
   here: it accepts a leading sign, so `\\u+12f` would slip through."
  [^long c]
  (cond (and (>= c 48) (<= c 57)) (- c 48)
        (and (>= c 97) (<= c 102)) (+ 10 (- c 97))
        (and (>= c 65) (<= c 70)) (+ 10 (- c 65))
        :else nil))

(defn- digit?
  "True for an ASCII `0`-`9`. `Character/isDigit` is not usable here: it accepts
   every Unicode decimal digit, so the fullwidth `1` (U+FF11) would parse as a
   number the grammar has no place for."
  [^long c]
  (and (>= c 48) (<= c 57)))

(defn read-str
  "Parse one JSON document out of `s`. Options: `:max-depth`.

   The whole string must be ONE value: trailing content that is not whitespace
   is a parse error, so a truncated stream cannot look like a valid document.
   Duplicate object keys resolve last-one-wins, which is what every JSON parser
   in practice does and what the conformance suite leaves open."
  ([^String s] (read-str s nil))
  ([^String s opts]
   (when (nil? s) (throw (ex-info "no JSON to read" {:skjema/error :json/parse :offset 0})))
   (let [len (.length s)
         max-depth (long (or (:max-depth opts) default-max-depth))
         pos (long-array 1)]
     (letfn [(at [] (aget pos 0))
             (advance! [] (aset pos 0 (long (inc (aget pos 0)))))
             (ws! []
               (loop [i (aget pos 0)]
                 (if (and (< i len)
                          (case (.charAt s i) (\space \tab \newline \return) true false))
                   (recur (inc i))
                   (aset pos 0 (long i)))))
             (ch []
               (let [i (aget pos 0)]
                 (when (< i len) (.charAt s i))))
             (expect! [^Character c what]
               (when-not (= c (ch)) (fail! s (at) (str "expected " what)))
               (advance!))
             (literal [^String word v]
               (let [i (at) end (+ i (.length word))]
                 (when (or (> end len) (not= word (.substring s i end)))
                   (fail! s i (str "expected " word)))
                 (aset pos 0 (long end))
                 v))
             (digits! [^long from]
               (loop [i from]
                 (if (and (< i len) (digit? (int (.charAt s i)))) (recur (inc i)) i)))
             (number []
               (let [start (at)
                     i (if (= \- (ch)) (inc start) start)
                     _ (when (>= i len) (fail! s i "number has no digits"))
                     c0 (.charAt s i)
                     i (cond (= c0 \0) (inc i)
                             (digit? (int c0)) (digits! (inc i))
                             :else (fail! s i "number has no digits"))
                     _ (when (and (= c0 \0) (< i len) (digit? (int (.charAt s i))))
                         (fail! s i "number has a leading zero"))
                     [i frac?] (if (and (< i len) (= \. (.charAt s i)))
                                 (let [d (digits! (inc i))]
                                   (when (= d (inc i)) (fail! s d "fraction has no digits"))
                                   [d true])
                                 [i false])
                     [i exp?] (if (and (< i len) (case (.charAt s i) (\e \E) true false))
                                (let [j (if (and (< (inc i) len)
                                                 (case (.charAt s (inc i)) (\+ \-) true false))
                                          (+ i 2)
                                          (inc i))
                                      d (digits! j)]
                                  (when (= d j) (fail! s d "exponent has no digits"))
                                  [d true])
                                [i false])
                     text (.substring s start i)]
                 (aset pos 0 (long i))
                 (if (or frac? exp?)
                   (Double/parseDouble text)
                   (try (Long/parseLong text)
                        (catch NumberFormatException _ (BigInteger. text))))))
             (text []
               (let [sb (StringBuilder.)]
                 (loop [i (inc (at))]
                   (when (>= i len) (fail! s i "unterminated string"))
                   (let [c (.charAt s i)]
                     (cond
                       (= c \") (do (aset pos 0 (long (inc i))) (.toString sb))
                       (= c \\)
                       (do (when (>= (inc i) len) (fail! s i "unterminated escape"))
                           (let [e (.charAt s (inc i))]
                             (if (= e \u)
                               (do (when (> (+ i 6) len) (fail! s i "truncated \\u escape"))
                                   (let [v (loop [k (+ i 2) acc 0]
                                             (if (= k (+ i 6))
                                               acc
                                               (if-let [d (hex-digit (int (.charAt s k)))]
                                                 (recur (inc k) (+ (* 16 acc) (long d)))
                                                 (fail! s k "\\u escape is not four hex digits"))))]
                                     (.append sb (char v))
                                     (recur (+ i 6))))
                               (do (.append sb (case e
                                                 \" \"
                                                 \\ \\
                                                 \/ \/
                                                 \b \backspace
                                                 \f \formfeed
                                                 \n \newline
                                                 \r \return
                                                 \t \tab
                                                 (fail! s i (str "invalid escape \\" e))))
                                   (recur (+ i 2))))))
                       (< (int c) 0x20) (fail! s i "unescaped control character in string")
                       :else (do (.append sb c) (recur (inc i))))))))
             (value [^long depth]
               (when (> depth max-depth) (fail! s (at) "document nests deeper than :max-depth"))
               (ws!)
               (let [c (ch)]
                 (when (nil? c) (fail! s (at) "unexpected end of input"))
                 (case c
                   \{ (object depth)
                   \[ (array depth)
                   \" (text)
                   \t (literal "true" true)
                   \f (literal "false" false)
                   \n (literal "null" nil)
                   (number))))
             (array [^long depth]
               (advance!)
               (ws!)
               (if (= \] (ch))
                 (do (advance!) [])
                 (loop [acc (transient [])]
                   (let [acc (conj! acc (value (inc depth)))]
                     (ws!)
                     (case (ch)
                       \, (do (advance!) (recur acc))
                       \] (do (advance!) (persistent! acc))
                       (fail! s (at) "expected ',' or ']'"))))))
             (object [^long depth]
               (advance!)
               (ws!)
               (if (= \} (ch))
                 (do (advance!) {})
                 (loop [acc (transient {})]
                   (ws!)
                   (when-not (= \" (ch)) (fail! s (at) "expected a string key"))
                   (let [k (text)]
                     (ws!)
                     (expect! \: "':' after a key")
                     (let [acc (assoc! acc k (value (inc depth)))]
                       (ws!)
                       (case (ch)
                         \, (do (advance!) (recur acc))
                         \} (do (advance!) (persistent! acc))
                         (fail! s (at) "expected ',' or '}'")))))))]
       (let [v (value 0)]
         (ws!)
         (when (< (at) len) (fail! s (at) "trailing content after the document"))
         v)))))

(defn read-bytes
  "Parse a JSON document from UTF-8 BYTES. RFC 8259 requires UTF-8, so malformed
   encoding is REJECTED here rather than silently replaced with U+FFFD - the
   replacement is what turns invalid input into a document that parses."
  ([^bytes b] (read-bytes b nil))
  ([^bytes b opts]
   (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                   (.onMalformedInput CodingErrorAction/REPORT)
                   (.onUnmappableCharacter CodingErrorAction/REPORT))
         s (try
             (.toString (.decode decoder (ByteBuffer/wrap b)))
             (catch CharacterCodingException e
               (throw (ex-info "JSON bytes are not valid UTF-8"
                               {:skjema/error :json/encoding} e))))]
     (read-str s opts))))

(defn- write-string!
  [^StringBuilder sb ^String s]
  (.append sb \")
  (dotimes [i (.length s)]
    (let [c (.charAt s i)]
      (case c
        \" (.append sb "\\\"")
        \\ (.append sb "\\\\")
        \backspace (.append sb "\\b")
        \formfeed (.append sb "\\f")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        (if (< (int c) 0x20)
          (.append sb (format "\\u%04x" (int c)))
          (.append sb c)))))
  (.append sb \"))

(defn- write-value!
  [^StringBuilder sb x]
  (cond
    (nil? x) (.append sb "null")
    (instance? Boolean x) (.append sb (if x "true" "false"))
    (string? x) (write-string! sb x)
    (or (keyword? x) (symbol? x)) (write-string! sb (name x))
    (number? x)
    (do (when (and (instance? Double x) (or (Double/isNaN x) (Double/isInfinite x)))
          (throw (ex-info "JSON has no NaN or Infinity" {:skjema/error :json/write :value x})))
        (when (ratio? x)
          (throw (ex-info "JSON has no ratios" {:skjema/error :json/write :value x})))
        (.append sb (str x)))
    (map? x)
    (do (.append sb \{)
        (reduce-kv (fn [first? k v]
                     (when-not first? (.append sb \,))
                     (cond (string? k) (write-string! sb k)
                           (or (keyword? k) (symbol? k)) (write-string! sb (name k))
                           :else (throw (ex-info "a JSON object key must be a string"
                                                 {:skjema/error :json/write :key k})))
                     (.append sb \:)
                     (write-value! sb v)
                     false)
                   true
                   x)
        (.append sb \}))
    (or (sequential? x) (instance? java.util.Collection x))
    (do (.append sb \[)
        (reduce (fn [first? v]
                  (when-not first? (.append sb \,))
                  (write-value! sb v)
                  false)
                true
                x)
        (.append sb \]))
    :else (throw (ex-info (str "no JSON representation for " (class x))
                          {:skjema/error :json/write :value x}))))

(defn write-str
  "Render Clojure data as JSON text. Keywords and symbols become strings (their
   `name`), which is how a validation result written back out keeps the
   specification's own key spelling. A value JSON cannot express - NaN, an
   infinity, a ratio, a non-string object key - is refused rather than coerced,
   because a silent coercion is a document nobody can validate."
  ^String [x]
  (let [sb (StringBuilder.)]
    (write-value! sb x)
    (.toString sb)))
