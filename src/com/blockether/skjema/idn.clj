(ns com.blockether.skjema.idn
  "Internationalized domain names, as IDNA2008 and UTS 46 define them - what
   `format: idn-hostname` asserts and what `format: idn-email` asks about the
   part after the at sign.

   A name is read the way a resolver reads it. First the whole string is
   MAPPED: the characters Unicode discards in a domain name are dropped, the
   compatibility forms are folded (so a fullwidth digit becomes an ASCII one)
   and everything is lower-cased. Then it is split on the four characters that
   separate labels - `.` and its ideographic, fullwidth and halfwidth twins.

   Each label is then either an A-label, `xn--` followed by Punycode, which is
   decoded and must re-encode to exactly what arrived, or a U-label, whose
   every code point must be PVALID by the derived property of RFC 5892 - and
   the handful that are only CONTEXTUALLY valid must satisfy their rule: a
   MIDDLE DOT between two `l`s, a Greek KERAIA before a Greek letter, a
   KATAKANA MIDDLE DOT in a label that has Japanese in it, a ZERO WIDTH JOINER
   after a virama, and Arabic-Indic digits that do not mix with their extended
   cousins. Finally, a name with any right-to-left label answers to the Bidi
   rule of RFC 5893, which is what makes `0a.<hebrew>` invalid while
   `<arabic><extended-indic-digit>` stays valid.

   Two of the tables Unicode publishes have no Java API - the derived property
   NFKC_CaseFold and Joining_Type - so the first is computed as
   `NFC(lower-case(NFKC(x)))` and the second from the script and category of
   the character, with the right-joining letters of the Arabic block written
   out. Both agree with the published data on everything a domain name can
   hold; neither is a substitute for the tables themselves."
  (:require [clojure.string :as str])
  (:import (java.text Normalizer Normalizer$Form)
           (java.util Locale)))

(set! *warn-on-reflection* true)

;; Punycode (RFC 3492)

(def ^:private p-base 36)
(def ^:private p-tmin 1)
(def ^:private p-tmax 26)
(def ^:private p-skew 38)
(def ^:private p-damp 700)
(def ^:private p-initial-bias 72)
(def ^:private p-initial-n 128)

(defn- adapt ^long [^long delta ^long numpoints first?]
  (let [delta (long (if first? (quot delta p-damp) (quot delta 2)))
        delta (long (+ delta (quot delta numpoints)))]
    (loop [delta delta k 0]
      (if (> delta (quot (* (- p-base p-tmin) p-tmax) 2))
        (recur (long (quot delta (- p-base p-tmin))) (long (+ k p-base)))
        (long (+ k (quot (* (inc (- p-base p-tmin)) delta) (+ delta p-skew))))))))

(defn- threshold ^long [^long k ^long bias]
  (let [t (- k bias)]
    (cond (< t p-tmin) p-tmin
          (> t p-tmax) p-tmax
          :else t)))

(defn- basic-digit
  "The value of one Punycode digit, or nil when the character is not one."
  [^long c]
  (cond (<= 48 c 57) (+ 26 (- c 48))
        (<= 97 c 122) (- c 97)
        (<= 65 c 90) (- c 65)
        :else nil))

(defn- digit-char ^long [^long d]
  (if (< d 26) (+ 97 d) (+ 48 (- d 26))))

(defn code-points
  "The code points of `s` as a vector, so a character outside the basic plane
   counts once and not twice."
  [^String s]
  (vec (.toArray (.codePoints s))))

(defn- from-code-points ^String [cps]
  (let [sb (StringBuilder.)]
    (doseq [cp cps] (.appendCodePoint sb (int cp)))
    (.toString sb)))

(defn decode
  "The string a Punycode body encodes, or nil when it is not Punycode."
  [^String s]
  (when-not (some #(>= (long %) 128) (code-points s))
    (let [delim (.lastIndexOf s "-")
          basic (if (pos? delim) (subs s 0 delim) "")
          ext (if (pos? delim) (subs s (inc delim)) (if (zero? delim) (subs s 1) s))
          ext-cps (code-points ext)
          n-ext (count ext-cps)]
      (loop [out (code-points basic), i 0, n (long p-initial-n), bias (long p-initial-bias), at 0]
        (if (>= at n-ext)
          (from-code-points out)
          (let [[i' at' ok?]
                (loop [i (long i), at (long at), k (long p-base), w 1]
                  (if (>= at n-ext)
                    [i at false]
                    (let [d (basic-digit (long (nth ext-cps at)))
                          t (threshold k bias)]
                      (cond
                        (nil? d) [i at false]
                        (> (* (long d) (long w)) 0x7fffffff) [i at false]
                        :else
                        (let [i (+ i (* (long d) (long w)))]
                          (if (< (long d) t)
                            [i (inc at) true]
                            (recur i (inc at) (+ k (long p-base)) (* w (- (long p-base) t)))))))))]
            (when ok?
              (let [len (inc (count out))
                    bias' (adapt (- i' i) len (zero? i))
                    n' (+ n (quot i' len))
                    pos (rem i' len)]
                (when (>= n' 128)
                  (when (Character/isValidCodePoint (int n'))
                    (recur (vec (concat (subvec out 0 pos) [n'] (subvec out pos)))
                           (long (inc pos)) (long n') (long bias') (long at'))))))))))))

(defn encode
  "`s` as the body of an A-label - Punycode, without the `xn--` prefix."
  ^String [^String s]
  (let [cps (code-points s)
        basic (filterv #(< (long %) 128) cps)
        b (count basic)
        sb (StringBuilder. (from-code-points basic))]
    (when (pos? b) (.append sb \-))
    (loop [n (long p-initial-n), delta 0, bias (long p-initial-bias), h (long b)]
      (if (>= h (count cps))
        (.toString sb)
        (let [m (long (reduce min (filter #(>= (long %) n) cps)))
              delta (+ delta (* (- m n) (inc h)))]
          (let [[delta bias h]
                (reduce (fn [[delta bias h] cp]
                          (let [cp (long cp)]
                            (cond
                              (< cp m) [(inc delta) bias h]
                              (> cp m) [delta bias h]
                              :else
                              (do (loop [q delta k p-base]
                                    (let [t (threshold k bias)]
                                      (if (< q t)
                                        (.appendCodePoint sb (int (digit-char q)))
                                        (do (.appendCodePoint
                                              sb (int (digit-char (+ t (rem (- q t) (- p-base t))))))
                                            (recur (quot (- q t) (- p-base t)) (+ k p-base))))))
                                  [0 (adapt delta (inc h) (= h b)) (inc h)]))))
                        [delta bias h]
                        cps)]
            (recur (inc m) (long (inc delta)) (long bias) (long h))))))))

;; The derived property of RFC 5892

(def ^:private exceptions
  "RFC 5892 section 2.6: the code points whose value the algorithm would get
   wrong, decided by hand once and for all."
  (merge {0x00DF :pvalid 0x03C2 :pvalid 0x06FD :pvalid 0x06FE :pvalid
          0x0F0B :pvalid 0x3007 :pvalid
          0x00B7 :contexto 0x0375 :contexto 0x05F3 :contexto 0x05F4 :contexto
          0x30FB :contexto
          0x0640 :disallowed 0x07FA :disallowed 0x302E :disallowed
          0x302F :disallowed 0x303B :disallowed}
         (zipmap (range 0x0660 0x066A) (repeat :contexto))
         (zipmap (range 0x06F0 0x06FA) (repeat :contexto))
         (zipmap (range 0x3031 0x3036) (repeat :disallowed))))

(def ^:private ignored-code-points
  "What the mapping step drops: the characters Unicode marks as commonly
   mapped to nothing. The two join controls are NOT among them - they carry
   meaning inside a label and answer to a contextual rule instead."
  (into #{0x00AD 0x034F 0x1806 0x180B 0x180C 0x180D 0x200B 0x2060 0xFEFF}
        (range 0xFE00 0xFE10)))

(defn- noncharacter? [^long cp]
  (or (<= 0xFDD0 cp 0xFDEF) (>= (bit-and cp 0xFFFE) 0xFFFE)))

(def ^:private default-ignorable
  (into #{0x00AD 0x034F 0x061C 0x115F 0x1160 0x17B4 0x17B5 0x3164 0xFEFF 0xFFA0}
        (concat (range 0x180B 0x180F) (range 0x200B 0x2010) (range 0x202A 0x202F)
                (range 0x2060 0x2070) (range 0xFE00 0xFE10) (range 0xFFF0 0xFFF9)
                (range 0x1D173 0x1D17B) (range 0xE0000 0xE1000))))

(defn- white-space? [^long cp]
  (or (<= 0x09 cp 0x0D) (= cp 0x85) (Character/isSpaceChar (int cp))))

(defn- old-hangul-jamo? [^long cp]
  (or (<= 0x1100 cp 0x11FF) (<= 0xA960 cp 0xA97C)
      (<= 0xD7B0 cp 0xD7C6) (<= 0xD7CB cp 0xD7FB)))

(def ^:private letter-digit-types
  (into #{} (map int) [Character/LOWERCASE_LETTER Character/UPPERCASE_LETTER
                       Character/OTHER_LETTER Character/MODIFIER_LETTER
                       Character/NON_SPACING_MARK Character/COMBINING_SPACING_MARK
                       Character/DECIMAL_DIGIT_NUMBER]))

(def ^:private mark-types
  (into #{} (map int) [Character/NON_SPACING_MARK Character/COMBINING_SPACING_MARK
                       Character/ENCLOSING_MARK]))

(defn- nfkc-case-fold
  "NFKC_CaseFold, as far as the JDK can answer it: compatibility composition,
   lower case, composed again."
  ^String [^String s]
  (Normalizer/normalize
    (.toLowerCase (Normalizer/normalize s Normalizer$Form/NFKC) Locale/ROOT)
    Normalizer$Form/NFC))

(defn- unstable? [^long cp]
  (let [s (from-code-points [cp])]
    (not= s (nfkc-case-fold s))))

(defn- derived-property
  "PVALID, CONTEXTJ, CONTEXTO or DISALLOWED for one code point."
  [^long cp]
  (or (exceptions cp)
      (cond
        (or (= cp 0x200C) (= cp 0x200D)) :contextj
        (or (<= 0x61 cp 0x7A) (<= 0x30 cp 0x39) (= cp 0x2D)) :pvalid
        (= (Character/getType (int cp)) (int Character/UNASSIGNED)) :disallowed
        (unstable? cp) :disallowed
        (or (default-ignorable cp) (white-space? cp) (noncharacter? cp)) :disallowed
        (old-hangul-jamo? cp) :disallowed
        (letter-digit-types (Character/getType (int cp))) :pvalid
        :else :disallowed)))

;; Contextual rules (RFC 5892 appendix A)

(def ^:private viramas
  "Every combining character with canonical combining class 9. The JDK does
   not publish the class, so the code points are written out."
  #{0x094D 0x09CD 0x0A4D 0x0ACD 0x0B4D 0x0BCD 0x0C4D 0x0CCD 0x0D3B 0x0D3C 0x0D4D
    0x0DCA 0x0E3A 0x0EBA 0x0F84 0x1039 0x103A 0x1714 0x1734 0x17D2 0x1A60 0x1B44
    0x1BAA 0x1BAB 0x1BF2 0x1BF3 0x2D7F 0xA806 0xA8C4 0xA953 0xA9C0 0xAAF6 0xABED
    0x10A3F 0x11046 0x1107F 0x110B9 0x11133 0x11134 0x111C0 0x11235 0x112EA
    0x1134D 0x11442 0x114C2 0x115BF 0x1163F 0x116B6 0x1172B 0x11839 0x119E0
    0x11A34 0x11A47 0x11A99 0x11C3F 0x11D44 0x11D45 0x11D97})

(def ^:private right-joining
  "The Joining_Type R letters of the Arabic block; every other letter of a
   cursive script is dual-joining as far as a domain name is concerned."
  (into #{0x0622 0x0623 0x0624 0x0625 0x0627 0x0629 0x062F 0x0630 0x0631 0x0632
          0x0648 0x0671 0x0672 0x0673 0x0675 0x0676 0x0677 0x06C0 0x06CD 0x06CF
          0x06D2 0x06D3 0x06D5 0x06EE 0x06EF 0x0710 0x0715 0x0716 0x0717 0x0718
          0x0719 0x071E 0x0728 0x072A 0x072C 0x072F 0x074D}
        (concat (range 0x0688 0x0700) (range 0x06C1 0x06CC))))

(def ^:private cursive-scripts
  #{java.lang.Character$UnicodeScript/ARABIC
    java.lang.Character$UnicodeScript/SYRIAC
    java.lang.Character$UnicodeScript/NKO
    java.lang.Character$UnicodeScript/MANDAIC
    java.lang.Character$UnicodeScript/MONGOLIAN
    java.lang.Character$UnicodeScript/MANICHAEAN
    java.lang.Character$UnicodeScript/PSALTER_PAHLAVI
    java.lang.Character$UnicodeScript/HANIFI_ROHINGYA
    java.lang.Character$UnicodeScript/SOGDIAN
    java.lang.Character$UnicodeScript/ADLAM})

(defn- joining-type [cp]
  (let [cp (long (or cp -1))]
    (cond
      (neg? cp) :none
      (#{(int Character/NON_SPACING_MARK) (int Character/ENCLOSING_MARK)
         (int Character/FORMAT)} (Character/getType (int cp)))
      (if (#{0x200C 0x200D} cp) :none :transparent)

      (right-joining cp) :right
      (and (cursive-scripts (java.lang.Character$UnicodeScript/of (int cp)))
           (#{(int Character/OTHER_LETTER) (int Character/MODIFIER_LETTER)}
             (Character/getType (int cp))))
      :dual

      :else :none)))

(defn- script? [cp script]
  (and cp (= (java.lang.Character$UnicodeScript/of (int cp))
             (java.lang.Character$UnicodeScript/forName script))))

(defn- skip-transparent [cps i step]
  (loop [i i]
    (if (= :transparent (joining-type (get cps i)))
      (recur (+ i step))
      (get cps i))))

(defn- contextual-ok?
  "Whether the code point at `i` satisfies the rule that lets it appear."
  [cps i]
  (let [cp (long (nth cps i))
        before (get cps (dec i))
        after (get cps (inc i))]
    (condp = cp
      0x200C (or (viramas (long (or before -1)))
                 (and (#{:left :dual} (joining-type (skip-transparent cps (dec i) -1)))
                      (#{:right :dual} (joining-type (skip-transparent cps (inc i) 1)))))
      0x200D (boolean (viramas (long (or before -1))))
      0x00B7 (and (= before 0x6C) (= after 0x6C))
      0x0375 (script? after "GREEK")
      0x05F3 (script? before "HEBREW")
      0x05F4 (script? before "HEBREW")
      0x30FB (boolean (some #(or (script? % "HIRAGANA") (script? % "KATAKANA")
                                 (script? % "HAN"))
                            cps))
      (cond
        (<= 0x0660 cp 0x0669) (not-any? #(<= 0x06F0 (long %) 0x06F9) cps)
        (<= 0x06F0 cp 0x06F9) (not-any? #(<= 0x0660 (long %) 0x0669) cps)
        :else true))))

;; The Bidi rule (RFC 5893)

(defn- directionality [cp] (int (Character/getDirectionality (int cp))))

(def ^:private dir-l (int Character/DIRECTIONALITY_LEFT_TO_RIGHT))
(def ^:private dir-r (int Character/DIRECTIONALITY_RIGHT_TO_LEFT))
(def ^:private dir-al (int Character/DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC))
(def ^:private dir-en (int Character/DIRECTIONALITY_EUROPEAN_NUMBER))
(def ^:private dir-an (int Character/DIRECTIONALITY_ARABIC_NUMBER))
(def ^:private dir-nsm (int Character/DIRECTIONALITY_NONSPACING_MARK))

(def ^:private rtl-allowed
  #{dir-r dir-al dir-an dir-en
    (int Character/DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR)
    (int Character/DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR)
    (int Character/DIRECTIONALITY_COMMON_NUMBER_SEPARATOR)
    (int Character/DIRECTIONALITY_OTHER_NEUTRALS)
    (int Character/DIRECTIONALITY_BOUNDARY_NEUTRAL)
    dir-nsm})

(def ^:private ltr-allowed
  #{dir-l dir-en
    (int Character/DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR)
    (int Character/DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR)
    (int Character/DIRECTIONALITY_COMMON_NUMBER_SEPARATOR)
    (int Character/DIRECTIONALITY_OTHER_NEUTRALS)
    (int Character/DIRECTIONALITY_BOUNDARY_NEUTRAL)
    dir-nsm})

(defn- rtl-label? [cps]
  (boolean (some #(#{dir-r dir-al dir-an} (directionality %)) cps)))

(defn- bidi-label-ok? [cps]
  (let [dirs (mapv directionality cps)
        trailing (->> (reverse dirs) (drop-while #(= % dir-nsm)) first)]
    (if (#{dir-r dir-al} (first dirs))
      (and (every? rtl-allowed dirs)
           (#{dir-r dir-al dir-en dir-an} trailing)
           (not (and (some #(= % dir-en) dirs) (some #(= % dir-an) dirs))))
      (and (= (first dirs) dir-l)
           (every? ltr-allowed dirs)
           (#{dir-l dir-en} trailing)))))

;; Labels and names

(def ^:private separators #{0x002E 0x3002 0xFF0E 0xFF61})

(defn- map-string
  "The UTS 46 mapping step: drop what a domain name ignores, fold the
   compatibility forms, lower-case, compose."
  ^String [^String s]
  (nfkc-case-fold (from-code-points (remove ignored-code-points (code-points s)))))

(defn- ldh-label? [^String label]
  (and (<= 1 (count label) 63)
       (re-matches #"[a-z0-9]([a-z0-9-]*[a-z0-9])?" label)))

(defn- u-label-ok?
  "Whether a decoded label is a U-label: the shape rules of RFC 5891 and the
   derived property of every code point."
  [cps]
  (let [n (count cps)]
    (and (pos? n)
         (not (mark-types (Character/getType (int (first cps)))))
         (not= 0x2D (long (first cps)))
         (not= 0x2D (long (last cps)))
         (not (and (> n 3) (= 0x2D (long (nth cps 2))) (= 0x2D (long (nth cps 3)))))
         (every? (fn [i]
                   (case (derived-property (long (nth cps i)))
                     :pvalid true
                     (:contextj :contexto) (contextual-ok? cps i)
                     false))
                 (range n)))))

(defn- label-points
  "The code points a label stands for: an A-label is decoded, and answers nil
   when its Punycode is not canonical."
  [^String label]
  (if (str/starts-with? label "xn--")
    (let [body (subs label 4)
          decoded (decode body)]
      (when (and decoded
                 (seq decoded)
                 (some #(>= (long %) 128) (code-points decoded))
                 (= body (encode decoded)))
        (code-points decoded)))
    (code-points label)))

(defn- a-label-length
  "How many octets the label takes in the DNS."
  ^long [^String label cps]
  (if (some #(>= (long %) 128) cps)
    (+ 4 (count (encode (from-code-points cps))))
    (count label)))

(defn hostname?
  "Whether `s` is an internationalized host name."
  [^String s]
  (let [mapped (map-string s)
        cps (code-points mapped)]
    (and (seq cps)
         (not-any? separators [(first cps) (last cps)])
         (let [labels (->> (partition-by #(boolean (separators %)) cps)
                           (remove #(separators (first %)))
                           (mapv vec))
               texts (mapv from-code-points labels)
               ;; every separator run must be a single one: `a..b` has none
               ;; between the two dots and is not a name.
               empty-label? (some #(> (count %) 1)
                                  (filter #(separators (first %)) (partition-by #(boolean (separators %)) cps)))
               points (mapv label-points texts)]
           (and (not empty-label?)
                (every? some? points)
                (every? (fn [[text cps]] (<= 1 (a-label-length text cps) 63))
                        (map vector texts points))
                (<= (reduce + (dec (count points)) (map a-label-length texts points)) 253)
                (every? (fn [[text cps]]
                          (if (every? #(< (long %) 128) cps)
                            (ldh-label? (from-code-points cps))
                            (u-label-ok? cps)))
                        (map vector texts points))
                (or (not-any? rtl-label? points)
                    (every? bidi-label-ok? points)))))))
