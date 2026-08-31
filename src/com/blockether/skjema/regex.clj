(ns com.blockether.skjema.regex
  "ECMAScript regular expressions in the dialect java.util.regex speaks.

   `pattern` and `patternProperties` hold ECMA-262 regular expressions, and
   Java's are ALMOST the same language. Every difference between the two is
   silent - the pattern compiles on both sides and matches different strings:

   - `\\s` is six ASCII characters in Java; in ECMAScript it is every space
     separator, both line terminators and the byte-order mark,
   - `\\v` is one vertical tab in ECMAScript and a whole class of vertical
     whitespace in Java,
   - `\\ca` and `\\cA` are the same control character in ECMAScript, which
     takes the letter modulo 32; Java exclusive-ors with 64 and answers `!`,
   - `\\b` inside a character class is a backspace in ECMAScript,
   - `\\0` is NUL in ECMAScript and the start of an octal escape in Java,
   - `\\p{Letter}` and `\\p{Script=Greek}` are `\\p{L}` and `\\p{IsGreek}`.

   One scan answers both questions this library asks of a pattern: how Java
   spells it, and whether it was ECMAScript in the first place - which is what
   `format: regex` asserts, and why `(?P<name>...)`, `(?#comment)` and the
   inline flags `(?i)` are refused even though Java understands two of them."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private general-categories
  "The long general-category names ECMAScript writes in `\\p{...}` and the short
   ones java.util.regex answers to. Java knows `\\p{L}`; it has never heard of
   `\\p{Letter}`."
  {"Letter" "L" "Lowercase_Letter" "Ll" "Uppercase_Letter" "Lu" "Titlecase_Letter" "Lt"
   "Modifier_Letter" "Lm" "Other_Letter" "Lo" "Cased_Letter" "LC"
   "Mark" "M" "Nonspacing_Mark" "Mn" "Spacing_Mark" "Mc" "Enclosing_Mark" "Me"
   "Number" "N" "Decimal_Number" "Nd" "Letter_Number" "Nl" "Other_Number" "No"
   "Punctuation" "P" "Connector_Punctuation" "Pc" "Dash_Punctuation" "Pd"
   "Open_Punctuation" "Ps" "Close_Punctuation" "Pe" "Initial_Punctuation" "Pi"
   "Final_Punctuation" "Pf" "Other_Punctuation" "Po"
   "Symbol" "S" "Math_Symbol" "Sm" "Currency_Symbol" "Sc" "Modifier_Symbol" "Sk"
   "Other_Symbol" "So"
   "Separator" "Z" "Space_Separator" "Zs" "Line_Separator" "Zl" "Paragraph_Separator" "Zp"
   "Other" "C" "Control" "Cc" "Format" "Cf" "Surrogate" "Cs" "Private_Use" "Co"
   "Unassigned" "Cn"})

(def ^:private short-category #"^[LMNPSZC][a-zA-Z]?$")

(defn- unicode-property
  "One `\\p{...}` body in java.util.regex spelling. A general category keeps its
   short name, a script or binary property takes Java's `Is` prefix."
  [body]
  (let [body (str/trim body)
        [k v] (if (str/includes? body "=")
                (let [[a b] (str/split body #"=" 2)] [(str/trim a) (str/trim b)])
                [nil body])]
    (cond
      (nil? k) (or (general-categories v)
                   (when (re-find short-category v) v)
                   (str "Is" v))
      (#{"General_Category" "gc"} k) (or (general-categories v) v)
      :else (str "Is" v))))

(def ^:private ecma-space
  "What ECMAScript's `\\s` matches - WhiteSpace, LineTerminator and the
   byte-order mark - written as the body of a java.util.regex class."
  "\\t\\n\\x0B\\f\\r \\u00a0\\u1680\\u2000-\\u200a\\u2028\\u2029\\u202f\\u205f\\u3000\\ufeff")

(def ^:private escape-letters
  "Every letter ECMAScript gives a meaning after a backslash. A letter outside
   this set is not an escape at all - `\\a` is a syntax error, not a bell."
  #{\d \D \w \W \s \S \b \B \f \n \r \t \v \c \x \u \k \p \P})

(defn- ascii-digit? [c] (and c (<= (int \0) (int c) (int \9))))

(defn- ascii-letter? [c]
  (and c (or (<= (int \a) (int c) (int \z)) (<= (int \A) (int c) (int \Z)))))

(defn- ascii-hex? [c]
  (and c (or (ascii-digit? c)
             (<= (int \a) (int c) (int \f))
             (<= (int \A) (int c) (int \F)))))

(defn scan
  "Read `p` once and answer `{:java <pattern> :error <reason or nil>}`.

   `:java` is always the closest java.util.regex spelling, so a pattern Java
   accepts still compiles even when ECMAScript would have refused it; `:error`
   is the first reason it was not an ECMAScript pattern, which is the whole
   judgement `format: regex` makes."
  [^String p]
  (let [n (.length p)
        sb (StringBuilder.)
        at (fn [i] (when (< i n) (.charAt p i)))
        put (fn [s] (.append sb (str s)) nil)]
    (loop [i 0, in-class? false, error nil]
      (if (>= i n)
        {:java (.toString sb)
         :error (or error (when in-class? "the character class is never closed"))}
        (let [c (.charAt p i)]
          (cond
            (= c \\)
            (let [d (at (inc i))]
              (cond
                (nil? d)
                (do (put c) (recur (inc i) in-class? (or error "the pattern ends in a backslash")))

                (= d \s)
                (do (put (if in-class? ecma-space (str "[" ecma-space "]")))
                    (recur (+ i 2) in-class? error))

                (= d \S)
                (do (put (str "[^" ecma-space "]")) (recur (+ i 2) in-class? error))

                (= d \v)
                (do (put "\\x0B") (recur (+ i 2) in-class? error))

                (and (= d \b) in-class?)
                (do (put "\\x08") (recur (+ i 2) in-class? error))

                (and (= d \0) (not (ascii-digit? (at (+ i 2)))))
                (do (put "\\x00") (recur (+ i 2) in-class? error))

                (= d \c)
                (let [l (at (+ i 2))]
                  (if (ascii-letter? l)
                    (do (put (str "\\c" (Character/toUpperCase ^char l)))
                        (recur (+ i 3) in-class? error))
                    (do (put "\\c")
                        (recur (+ i 2) in-class?
                               (or error "a control escape needs a letter after it")))))

                (or (= d \p) (= d \P))
                (let [close (when (= (at (+ i 2)) \{) (.indexOf p "}" (int (+ i 3))))]
                  (if (and close (pos? (long close)))
                    (do (put (str "\\" d "{" (unicode-property (subs p (+ i 3) (long close))) "}"))
                        (recur (inc (long close)) in-class? error))
                    (do (put (str "\\" d))
                        (recur (+ i 2) in-class?
                               (or error "a property escape needs a {name} after it")))))

                (= d \x)
                (if (and (ascii-hex? (at (+ i 2))) (ascii-hex? (at (+ i 3))))
                  (do (put (str "\\x" (subs p (+ i 2) (+ i 4)))) (recur (+ i 4) in-class? error))
                  (do (put "\\x")
                      (recur (+ i 2) in-class?
                             (or error "a hexadecimal escape needs two digits"))))

                (= d \u)
                (let [close (when (= (at (+ i 2)) \{) (.indexOf p "}" (int (+ i 3))))]
                  (cond
                    (and close (pos? (long close)))
                    (do (put (str "\\x{" (subs p (+ i 3) (long close)) "}"))
                        (recur (inc (long close)) in-class? error))

                    (every? ascii-hex? [(at (+ i 2)) (at (+ i 3)) (at (+ i 4)) (at (+ i 5))])
                    (do (put (str "\\u" (subs p (+ i 2) (+ i 6)))) (recur (+ i 6) in-class? error))

                    :else
                    (do (put "\\u")
                        (recur (+ i 2) in-class?
                               (or error "a unicode escape needs four digits or {digits}")))))

                (= d \k)
                (if (= (at (+ i 2)) \<)
                  (do (put "\\k<") (recur (+ i 3) in-class? error))
                  (do (put "\\k")
                      (recur (+ i 2) in-class?
                             (or error "a back reference needs a <name> after it"))))

                (escape-letters d)
                (do (put (str "\\" d)) (recur (+ i 2) in-class? error))

                (ascii-digit? d)
                (do (put (str "\\" d)) (recur (+ i 2) in-class? error))

                (ascii-letter? d)
                (do (put (str "\\" d))
                    (recur (+ i 2) in-class?
                           (or error (str "`\\" d "` is not an ECMAScript escape"))))

                :else
                (do (put (str "\\" d)) (recur (+ i 2) in-class? error))))

            ;; ECMAScript has an empty class, which matches nothing, and its
            ;; negation, which matches anything; java.util.regex has neither.
            (and (= c \[) (not in-class?) (= (at (inc i)) \]))
            (do (put "[^\\x{0}-\\x{10FFFF}]") (recur (+ i 2) false error))

            (and (= c \[) (not in-class?) (= (at (inc i)) \^) (= (at (+ i 2)) \]))
            (do (put "[\\x{0}-\\x{10FFFF}]") (recur (+ i 3) false error))

            (and (= c \[) (not in-class?))
            (do (put c) (recur (inc i) true error))

            (and (= c \]) in-class?)
            (do (put c) (recur (inc i) false error))

            (and (= c \() (not in-class?))
            (if (= (at (inc i)) \?)
              (let [e (at (+ i 2))]
                (cond
                  (#{\: \= \!} e) (do (put (str "(?" e)) (recur (+ i 3) false error))
                  (= e \<) (if (#{\= \!} (at (+ i 3)))
                             (do (put (str "(?<" (at (+ i 3)))) (recur (+ i 4) false error))
                             (do (put "(?<") (recur (+ i 3) false error)))
                  :else (do (put "(?")
                            (recur (+ i 2) false
                                   (or error (str "`(?" e "` is not an ECMAScript group"))))))
              (do (put c) (recur (inc i) false error)))

            :else
            (do (put c) (recur (inc i) in-class? error))))))))

(defn translate
  "The java.util.regex spelling of the ECMAScript pattern `p`."
  [^String p]
  (:java (scan p)))

(def pattern-of
  "`p` translated and compiled, memoized: `patternProperties` matches the same
   handful of patterns against every property name of every instance."
  (memoize (fn [^String p] (re-pattern (translate p)))))

(defn ecma?
  "Whether `p` is an ECMAScript regular expression, which is what `format:
   regex` asserts. Compiling under Java is not enough on its own: Java accepts
   `(?i)`, `\\a` and refuses `(?#comment)` for reasons of its own."
  [^String p]
  (let [{:keys [java error]} (scan p)]
    (and (nil? error)
         (try (boolean (re-pattern java))
              (catch Exception _ false)))))
