package com.blockether.skjema;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;

/**
 * Internationalized domain names, as IDNA2008 and UTS 46 define them - what
 * {@code format: idn-hostname} asserts and what {@code format: idn-email} asks
 * about the part after the at sign.
 *
 * <p>A name is read the way a resolver reads it. First the whole string is
 * MAPPED: the characters Unicode discards in a domain name are dropped, the
 * compatibility forms are folded (so a fullwidth digit becomes an ASCII one)
 * and everything is lower-cased. Then it is split on the four characters that
 * separate labels - {@code .} and its ideographic, fullwidth and halfwidth
 * twins.
 *
 * <p>Each label is then either an A-label, {@code xn--} followed by Punycode,
 * which is decoded and must re-encode to exactly what arrived, or a U-label,
 * whose every code point must be PVALID by the derived property of RFC 5892 -
 * and the handful that are only CONTEXTUALLY valid must satisfy their rule: a
 * MIDDLE DOT between two {@code l}s, a Greek KERAIA before a Greek letter, a
 * KATAKANA MIDDLE DOT in a label that has Japanese in it, a ZERO WIDTH JOINER
 * after a virama, and Arabic-Indic digits that do not mix with their extended
 * cousins. Finally, a name with any right-to-left label answers to the Bidi
 * rule of RFC 5893, which is what makes {@code 0a.<hebrew>} invalid while
 * {@code <arabic><extended-indic-digit>} stays valid.
 *
 * <p>Two of the tables Unicode publishes have no Java API - the derived
 * property NFKC_CaseFold and Joining_Type - so the first is computed as
 * {@code NFC(lower-case(NFKC(x)))} and the second from the script and category
 * of the character, with the right-joining letters of the Arabic block written
 * out. Both agree with the published data on everything a domain name can
 * hold; neither is a substitute for the tables themselves.
 */
public final class Idn {
    private Idn() {}

    // The derived property of RFC 5892, and the three answers that are not PVALID.

    private static final int PVALID = 0;
    private static final int CONTEXTJ = 1;
    private static final int CONTEXTO = 2;
    private static final int DISALLOWED = 3;

    // Joining_Type, as far as a domain name needs it. No letter a name may hold
    // is left-joining only, so the ZWNJ rule asks about dual on both sides.

    private static final int JOIN_NONE = 0;
    private static final int JOIN_TRANSPARENT = 1;
    private static final int JOIN_RIGHT = 2;
    private static final int JOIN_DUAL = 3;

    private static final int P_BASE = 36;
    private static final int P_TMIN = 1;
    private static final int P_TMAX = 26;
    private static final int P_SKEW = 38;
    private static final int P_DAMP = 700;
    private static final int P_INITIAL_BIAS = 72;
    private static final int P_INITIAL_N = 128;

    /**
     * Every combining character with canonical combining class 9. The JDK does
     * not publish the class, so the code points are written out.
     */
    private static final int[] VIRAMAS = {
        0x094D, 0x09CD, 0x0A4D, 0x0ACD, 0x0B4D, 0x0BCD, 0x0C4D, 0x0CCD, 0x0D3B, 0x0D3C, 0x0D4D,
        0x0DCA, 0x0E3A, 0x0EBA, 0x0F84, 0x1039, 0x103A, 0x1714, 0x1734, 0x17D2, 0x1A60, 0x1B44,
        0x1BAA, 0x1BAB, 0x1BF2, 0x1BF3, 0x2D7F, 0xA806, 0xA8C4, 0xA953, 0xA9C0, 0xAAF6, 0xABED,
        0x10A3F, 0x11046, 0x1107F, 0x110B9, 0x11133, 0x11134, 0x111C0, 0x11235, 0x112EA,
        0x1134D, 0x11442, 0x114C2, 0x115BF, 0x1163F, 0x116B6, 0x1172B, 0x11839, 0x119E0,
        0x11A34, 0x11A47, 0x11A99, 0x11C3F, 0x11D44, 0x11D45, 0x11D97
    };

    /**
     * The Joining_Type R letters of the Arabic block that stand outside the two
     * ranges; every other letter of a cursive script is dual-joining as far as a
     * domain name is concerned.
     */
    private static final int[] RIGHT_JOINING = {
        0x0622, 0x0623, 0x0624, 0x0625, 0x0627, 0x0629, 0x062F, 0x0630, 0x0631, 0x0632,
        0x0648, 0x0671, 0x0672, 0x0673, 0x0675, 0x0676, 0x0677, 0x0710, 0x0715, 0x0716,
        0x0717, 0x0718, 0x0719, 0x071E, 0x0728, 0x072A, 0x072C, 0x072F, 0x074D
    };

    private static final EnumSet<Character.UnicodeScript> CURSIVE = EnumSet.of(
        Character.UnicodeScript.ARABIC,
        Character.UnicodeScript.SYRIAC,
        Character.UnicodeScript.NKO,
        Character.UnicodeScript.MANDAIC,
        Character.UnicodeScript.MONGOLIAN,
        Character.UnicodeScript.MANICHAEAN,
        Character.UnicodeScript.PSALTER_PAHLAVI,
        Character.UnicodeScript.HANIFI_ROHINGYA,
        Character.UnicodeScript.SOGDIAN,
        Character.UnicodeScript.ADLAM);

    static {
        Arrays.sort(VIRAMAS);
        Arrays.sort(RIGHT_JOINING);
    }

    /** Whether `s` is an internationalized host name. */
    public static boolean hostname(String s) {
        String mapped = mapString(s);
        if (plainAsciiHostname(mapped)) return true;

        int[] cps = codePoints(mapped);
        if (cps.length == 0) return false;
        if (isSeparator(cps[0]) || isSeparator(cps[cps.length - 1])) return false;

        // Every separator run must be a single one: `a..b` has no label between
        // the two dots and is not a name.
        List<int[]> labels = new ArrayList<>();
        int start = 0;
        for (int i = 0; i <= cps.length; i++) {
            if (i == cps.length || isSeparator(cps[i])) {
                if (i == start) return false;
                labels.add(Arrays.copyOfRange(cps, start, i));
                start = i + 1;
            }
        }

        long total = labels.size() - 1L;
        boolean rightToLeft = false;
        int[][] points = new int[labels.size()][];
        for (int i = 0; i < labels.size(); i++) {
            int[] label = labels.get(i);
            int[] decoded = labelPoints(label);
            if (decoded == null) return false;
            long length = aLabelLength(label, decoded);
            if (length < 1 || length > 63) return false;
            total += length;
            points[i] = decoded;
            rightToLeft |= isRightToLeftLabel(decoded);
        }
        if (total > 253) return false;

        for (int[] decoded : points) {
            if (isAscii(decoded) ? !ldhLabel(decoded) : !uLabelOk(decoded)) return false;
            if (rightToLeft && !bidiLabelOk(decoded)) return false;
        }
        return true;
    }

    // Mapping

    /** `.` and its ideographic, fullwidth and halfwidth twins. */
    private static boolean isSeparator(int cp) {
        return cp == 0x002E || cp == 0x3002 || cp == 0xFF0E || cp == 0xFF61;
    }

    /**
     * What the mapping step drops: the characters Unicode marks as commonly
     * mapped to nothing. The two join controls are NOT among them - they carry
     * meaning inside a label and answer to a contextual rule instead.
     */
    private static boolean isIgnored(int cp) {
        return cp == 0x00AD || cp == 0x034F || cp == 0x1806 || cp == 0x200B || cp == 0x2060
            || cp == 0xFEFF || (cp >= 0x180B && cp <= 0x180D) || (cp >= 0xFE00 && cp <= 0xFE0F);
    }

    /**
     * NFKC_CaseFold, as far as the JDK can answer it: compatibility composition,
     * lower case, composed again.
     */
    private static String nfkcCaseFold(String s) {
        String folded = Normalizer.normalize(s, Normalizer.Form.NFKC).toLowerCase(Locale.ROOT);
        return Normalizer.normalize(folded, Normalizer.Form.NFC);
    }

    /**
     * The UTS 46 mapping step: drop what a domain name ignores, fold the
     * compatibility forms, lower-case, compose. An ASCII name - which every
     * A-label is, `xn--` and all - has nothing to drop and nothing to compose,
     * and its case fold is the ASCII one, so it skips both normalizations.
     */
    private static String mapString(String s) {
        if (isAscii(s)) return s.toLowerCase(Locale.ROOT);
        StringBuilder kept = new StringBuilder(s.length());
        for (int i = 0; i < s.length();) {
            int cp = s.codePointAt(i);
            if (!isIgnored(cp)) kept.appendCodePoint(cp);
            i += Character.charCount(cp);
        }
        return nfkcCaseFold(kept.toString());
    }

    // The derived property of RFC 5892

    /**
     * RFC 5892 section 2.6: the code points whose value the algorithm would get
     * wrong, decided by hand once and for all. -1 when there is no exception.
     */
    private static int exception(int cp) {
        switch (cp) {
            case 0x00DF: case 0x03C2: case 0x06FD: case 0x06FE: case 0x0F0B: case 0x3007:
                return PVALID;
            case 0x00B7: case 0x0375: case 0x05F3: case 0x05F4: case 0x30FB:
                return CONTEXTO;
            case 0x0640: case 0x07FA: case 0x302E: case 0x302F: case 0x303B:
                return DISALLOWED;
            default:
                if (cp >= 0x0660 && cp <= 0x0669) return CONTEXTO;
                if (cp >= 0x06F0 && cp <= 0x06F9) return CONTEXTO;
                if (cp >= 0x3031 && cp <= 0x3035) return DISALLOWED;
                return -1;
        }
    }

    private static boolean isNoncharacter(int cp) {
        return (cp >= 0xFDD0 && cp <= 0xFDEF) || (cp & 0xFFFE) == 0xFFFE;
    }

    private static boolean isDefaultIgnorable(int cp) {
        switch (cp) {
            case 0x00AD: case 0x034F: case 0x061C: case 0x115F: case 0x1160: case 0x17B4:
            case 0x17B5: case 0x3164: case 0xFEFF: case 0xFFA0:
                return true;
            default:
                return (cp >= 0x180B && cp <= 0x180E) || (cp >= 0x200B && cp <= 0x200F)
                    || (cp >= 0x202A && cp <= 0x202E) || (cp >= 0x2060 && cp <= 0x206F)
                    || (cp >= 0xFE00 && cp <= 0xFE0F) || (cp >= 0xFFF0 && cp <= 0xFFF8)
                    || (cp >= 0x1D173 && cp <= 0x1D17A) || (cp >= 0xE0000 && cp <= 0xE0FFF);
        }
    }

    private static boolean isWhiteSpace(int cp) {
        return (cp >= 0x09 && cp <= 0x0D) || cp == 0x85 || Character.isSpaceChar(cp);
    }

    private static boolean isOldHangulJamo(int cp) {
        return (cp >= 0x1100 && cp <= 0x11FF) || (cp >= 0xA960 && cp <= 0xA97C)
            || (cp >= 0xD7B0 && cp <= 0xD7C6) || (cp >= 0xD7CB && cp <= 0xD7FB);
    }

    private static boolean isLetterOrDigitType(int type) {
        return type == Character.LOWERCASE_LETTER || type == Character.UPPERCASE_LETTER
            || type == Character.OTHER_LETTER || type == Character.MODIFIER_LETTER
            || type == Character.NON_SPACING_MARK || type == Character.COMBINING_SPACING_MARK
            || type == Character.DECIMAL_DIGIT_NUMBER;
    }

    private static boolean isMarkType(int type) {
        return type == Character.NON_SPACING_MARK || type == Character.COMBINING_SPACING_MARK
            || type == Character.ENCLOSING_MARK;
    }

    /** Whether the mapping step would change the one code point. */
    private static boolean isUnstable(int cp) {
        String one = new String(Character.toChars(cp));
        return !one.equals(nfkcCaseFold(one));
    }

    /** PVALID, CONTEXTJ, CONTEXTO or DISALLOWED for one code point. */
    private static int derivedProperty(int cp) {
        int exception = exception(cp);
        if (exception >= 0) return exception;
        if (cp == 0x200C || cp == 0x200D) return CONTEXTJ;
        if ((cp >= 0x61 && cp <= 0x7A) || (cp >= 0x30 && cp <= 0x39) || cp == 0x2D) return PVALID;
        int type = Character.getType(cp);
        if (type == Character.UNASSIGNED) return DISALLOWED;
        if (isUnstable(cp)) return DISALLOWED;
        if (isDefaultIgnorable(cp) || isWhiteSpace(cp) || isNoncharacter(cp)) return DISALLOWED;
        if (isOldHangulJamo(cp)) return DISALLOWED;
        return isLetterOrDigitType(type) ? PVALID : DISALLOWED;
    }

    // Contextual rules (RFC 5892 appendix A)

    private static boolean isVirama(int cp) {
        return cp >= 0 && Arrays.binarySearch(VIRAMAS, cp) >= 0;
    }

    private static boolean isRightJoining(int cp) {
        return (cp >= 0x0688 && cp <= 0x06FF) || (cp >= 0x06C1 && cp <= 0x06CB)
            || Arrays.binarySearch(RIGHT_JOINING, cp) >= 0;
    }

    private static int joiningType(int cp) {
        if (cp < 0) return JOIN_NONE;
        int type = Character.getType(cp);
        if (type == Character.NON_SPACING_MARK || type == Character.ENCLOSING_MARK
                || type == Character.FORMAT) {
            return cp == 0x200C || cp == 0x200D ? JOIN_NONE : JOIN_TRANSPARENT;
        }
        if (isRightJoining(cp)) return JOIN_RIGHT;
        if (CURSIVE.contains(Character.UnicodeScript.of(cp))
                && (type == Character.OTHER_LETTER || type == Character.MODIFIER_LETTER)) {
            return JOIN_DUAL;
        }
        return JOIN_NONE;
    }

    private static boolean isScript(int cp, Character.UnicodeScript script) {
        return cp >= 0 && Character.UnicodeScript.of(cp) == script;
    }

    /** The first code point either side that a joining rule actually looks at. */
    private static int skipTransparent(int[] cps, int from, int step) {
        for (int i = from; i >= 0 && i < cps.length; i += step) {
            if (joiningType(cps[i]) != JOIN_TRANSPARENT) return cps[i];
        }
        return -1;
    }

    private static boolean joinsLeftward(int cp) {
        return joiningType(cp) == JOIN_DUAL;
    }

    private static boolean joinsRightward(int cp) {
        int type = joiningType(cp);
        return type == JOIN_RIGHT || type == JOIN_DUAL;
    }

    /** Whether the code point at `i` satisfies the rule that lets it appear. */
    private static boolean contextualOk(int[] cps, int i) {
        int cp = cps[i];
        int before = i > 0 ? cps[i - 1] : -1;
        int after = i + 1 < cps.length ? cps[i + 1] : -1;
        switch (cp) {
            case 0x200C:
                return isVirama(before)
                    || (joinsLeftward(skipTransparent(cps, i - 1, -1))
                        && joinsRightward(skipTransparent(cps, i + 1, 1)));
            case 0x200D:
                return isVirama(before);
            case 0x00B7:
                return before == 0x6C && after == 0x6C;
            case 0x0375:
                return isScript(after, Character.UnicodeScript.GREEK);
            case 0x05F3:
            case 0x05F4:
                return isScript(before, Character.UnicodeScript.HEBREW);
            case 0x30FB:
                for (int other : cps) {
                    if (isScript(other, Character.UnicodeScript.HIRAGANA)
                            || isScript(other, Character.UnicodeScript.KATAKANA)
                            || isScript(other, Character.UnicodeScript.HAN)) {
                        return true;
                    }
                }
                return false;
            default:
                if (cp >= 0x0660 && cp <= 0x0669) {
                    for (int other : cps) if (other >= 0x06F0 && other <= 0x06F9) return false;
                    return true;
                }
                if (cp >= 0x06F0 && cp <= 0x06F9) {
                    for (int other : cps) if (other >= 0x0660 && other <= 0x0669) return false;
                    return true;
                }
                return true;
        }
    }

    // The Bidi rule (RFC 5893)

    private static boolean isRightToLeftLabel(int[] cps) {
        for (int cp : cps) {
            byte direction = Character.getDirectionality(cp);
            if (direction == Character.DIRECTIONALITY_RIGHT_TO_LEFT
                    || direction == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC
                    || direction == Character.DIRECTIONALITY_ARABIC_NUMBER) {
                return true;
            }
        }
        return false;
    }

    private static boolean rtlAllowed(byte direction) {
        return direction == Character.DIRECTIONALITY_RIGHT_TO_LEFT
            || direction == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC
            || direction == Character.DIRECTIONALITY_ARABIC_NUMBER
            || direction == Character.DIRECTIONALITY_EUROPEAN_NUMBER
            || neutral(direction);
    }

    private static boolean ltrAllowed(byte direction) {
        return direction == Character.DIRECTIONALITY_LEFT_TO_RIGHT
            || direction == Character.DIRECTIONALITY_EUROPEAN_NUMBER
            || neutral(direction);
    }

    private static boolean neutral(byte direction) {
        return direction == Character.DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR
            || direction == Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
            || direction == Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR
            || direction == Character.DIRECTIONALITY_OTHER_NEUTRALS
            || direction == Character.DIRECTIONALITY_BOUNDARY_NEUTRAL
            || direction == Character.DIRECTIONALITY_NONSPACING_MARK;
    }

    private static boolean bidiLabelOk(int[] cps) {
        int last = cps.length - 1;
        while (last >= 0
                && Character.getDirectionality(cps[last]) == Character.DIRECTIONALITY_NONSPACING_MARK) {
            last--;
        }
        byte trailing = last >= 0 ? Character.getDirectionality(cps[last]) : -1;
        byte first = Character.getDirectionality(cps[0]);

        if (first == Character.DIRECTIONALITY_RIGHT_TO_LEFT
                || first == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC) {
            boolean european = false;
            boolean arabic = false;
            for (int cp : cps) {
                byte direction = Character.getDirectionality(cp);
                if (!rtlAllowed(direction)) return false;
                european |= direction == Character.DIRECTIONALITY_EUROPEAN_NUMBER;
                arabic |= direction == Character.DIRECTIONALITY_ARABIC_NUMBER;
            }
            if (european && arabic) return false;
            return trailing == Character.DIRECTIONALITY_RIGHT_TO_LEFT
                || trailing == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC
                || trailing == Character.DIRECTIONALITY_EUROPEAN_NUMBER
                || trailing == Character.DIRECTIONALITY_ARABIC_NUMBER;
        }

        if (first != Character.DIRECTIONALITY_LEFT_TO_RIGHT) return false;
        for (int cp : cps) if (!ltrAllowed(Character.getDirectionality(cp))) return false;
        return trailing == Character.DIRECTIONALITY_LEFT_TO_RIGHT
            || trailing == Character.DIRECTIONALITY_EUROPEAN_NUMBER;
    }

    // Labels

    private static int[] codePoints(String s) {
        return s.codePoints().toArray();
    }

    private static String fromCodePoints(int[] points) {
        StringBuilder result = new StringBuilder(points.length);
        for (int cp : points) result.appendCodePoint(cp);
        return result.toString();
    }

    private static boolean isAscii(String s) {
        for (int i = 0; i < s.length(); i++) if (s.charAt(i) > 127) return false;
        return true;
    }

    private static boolean isAscii(int[] cps) {
        for (int cp : cps) if (cp >= 128) return false;
        return true;
    }

    /**
     * The code points a label stands for: an A-label is decoded, and answers
     * null when its Punycode is not canonical.
     */
    private static int[] labelPoints(int[] label) {
        if (!(label.length > 4 && label[0] == 'x' && label[1] == 'n'
                && label[2] == '-' && label[3] == '-')) {
            return label;
        }
        String body = fromCodePoints(Arrays.copyOfRange(label, 4, label.length));
        String decoded = punycodeDecode(body);
        if (decoded == null || decoded.isEmpty()) return null;
        int[] cps = codePoints(decoded);
        if (isAscii(cps) || !body.equals(punycodeEncode(decoded))) return null;
        return cps;
    }

    /** How many octets the label takes in the DNS. */
    private static long aLabelLength(int[] label, int[] cps) {
        if (isAscii(cps)) return label.length;
        return 4L + punycodeEncode(fromCodePoints(cps)).length();
    }

    /** RFC 1123: letters, digits and an interior hyphen. */
    private static boolean ldhLabel(int[] cps) {
        int n = cps.length;
        if (n < 1 || n > 63) return false;
        for (int i = 0; i < n; i++) {
            int cp = cps[i];
            boolean alpha = cp >= 'a' && cp <= 'z';
            boolean digit = cp >= '0' && cp <= '9';
            boolean hyphen = cp == '-' && i != 0 && i != n - 1;
            if (!(alpha || digit || hyphen)) return false;
        }
        return true;
    }

    /**
     * Whether a decoded label is a U-label: the shape rules of RFC 5891 and the
     * derived property of every code point.
     */
    private static boolean uLabelOk(int[] cps) {
        int n = cps.length;
        if (n == 0) return false;
        if (isMarkType(Character.getType(cps[0]))) return false;
        if (cps[0] == 0x2D || cps[n - 1] == 0x2D) return false;
        if (n > 3 && cps[2] == 0x2D && cps[3] == 0x2D) return false;
        for (int i = 0; i < n; i++) {
            switch (derivedProperty(cps[i])) {
                case PVALID:
                    break;
                case CONTEXTJ:
                case CONTEXTO:
                    if (!contextualOk(cps, i)) return false;
                    break;
                default:
                    return false;
            }
        }
        return true;
    }

    /** RFC 1123's allocation-free ASCII path; false takes the complete IDN one. */
    private static boolean plainAsciiHostname(String value) {
        int n = value.length();
        if (n < 1 || n > 253) return false;
        int start = 0;
        for (int i = 0; i <= n; i++) {
            if (i == n || value.charAt(i) == '.') {
                int len = i - start;
                if (len < 1 || len > 63 || startsWithPunycode(value, start, i)) return false;
                for (int j = start; j < i; j++) {
                    char c = value.charAt(j);
                    boolean alpha = c >= 'a' && c <= 'z';
                    boolean digit = c >= '0' && c <= '9';
                    boolean hyphen = c == '-' && j != start && j != i - 1;
                    if (!(alpha || digit || hyphen)) return false;
                }
                start = i + 1;
            } else if (value.charAt(i) >= 128) {
                return false;
            }
        }
        return true;
    }

    private static boolean startsWithPunycode(String value, int start, int end) {
        return end - start >= 4
                && value.charAt(start) == 'x'
                && value.charAt(start + 1) == 'n'
                && value.charAt(start + 2) == '-'
                && value.charAt(start + 3) == '-';
    }

    // Punycode (RFC 3492)

    /** Decode one RFC 3492 Punycode body, or null when malformed. */
    public static String punycodeDecode(String input) {
        for (int offset = 0; offset < input.length();) {
            int cp = input.codePointAt(offset);
            if (cp >= 128) return null;
            offset += Character.charCount(cp);
        }
        int delimiter = input.lastIndexOf('-');
        ArrayList<Integer> output = new ArrayList<>();
        int at;
        if (delimiter > 0) {
            for (int i = 0; i < delimiter; i++) output.add((int) input.charAt(i));
            at = delimiter + 1;
        } else {
            at = delimiter == 0 ? 1 : 0;
        }
        long oldI;
        long i = 0;
        long n = P_INITIAL_N;
        long bias = P_INITIAL_BIAS;
        while (at < input.length()) {
            oldI = i;
            long weight = 1;
            for (long k = P_BASE;; k += P_BASE) {
                if (at >= input.length()) return null;
                int digit = basicDigit(input.charAt(at++));
                if (digit < 0 || digit > (Integer.MAX_VALUE - i) / weight) return null;
                i += digit * weight;
                long threshold = threshold(k, bias);
                if (digit < threshold) break;
                long factor = P_BASE - threshold;
                if (weight > Integer.MAX_VALUE / factor) return null;
                weight *= factor;
            }
            int length = output.size() + 1;
            bias = adapt(i - oldI, length, oldI == 0);
            n += i / length;
            if (!Character.isValidCodePoint((int) n) || n < 128) return null;
            int position = (int) (i % length);
            output.add(position, (int) n);
            i = position + 1L;
        }
        StringBuilder result = new StringBuilder(output.size());
        for (int cp : output) result.appendCodePoint(cp);
        return result.toString();
    }

    /** Encode one Unicode label as an RFC 3492 body, without the xn-- prefix. */
    public static String punycodeEncode(String input) {
        int[] points = input.codePoints().toArray();
        StringBuilder output = new StringBuilder(points.length + 8);
        int basic = 0;
        for (int cp : points) {
            if (cp < 128) {
                output.appendCodePoint(cp);
                basic++;
            }
        }
        if (basic > 0) output.append('-');
        long n = P_INITIAL_N;
        long delta = 0;
        long bias = P_INITIAL_BIAS;
        int handled = basic;
        while (handled < points.length) {
            long next = Long.MAX_VALUE;
            for (int cp : points) if (cp >= n && cp < next) next = cp;
            if (next == Long.MAX_VALUE || next - n > (Long.MAX_VALUE - delta) / (handled + 1L)) {
                throw new IllegalArgumentException("punycode input is too large");
            }
            delta += (next - n) * (handled + 1L);
            n = next;
            for (int cp : points) {
                if (cp < n) {
                    if (delta == Long.MAX_VALUE) {
                        throw new IllegalArgumentException("punycode input is too large");
                    }
                    delta++;
                } else if (cp == n) {
                    long q = delta;
                    for (long k = P_BASE;; k += P_BASE) {
                        long t = threshold(k, bias);
                        if (q < t) break;
                        output.appendCodePoint(digitChar(t + ((q - t) % (P_BASE - t))));
                        q = (q - t) / (P_BASE - t);
                    }
                    output.appendCodePoint(digitChar(q));
                    bias = adapt(delta, handled + 1L, handled == basic);
                    delta = 0;
                    handled++;
                }
            }
            delta++;
            n++;
        }
        return output.toString();
    }

    private static long adapt(long delta, long points, boolean first) {
        delta = first ? delta / P_DAMP : delta / 2;
        delta += delta / points;
        long k = 0;
        while (delta > ((P_BASE - P_TMIN) * P_TMAX) / 2) {
            delta /= P_BASE - P_TMIN;
            k += P_BASE;
        }
        return k + ((P_BASE - P_TMIN + 1L) * delta) / (delta + P_SKEW);
    }

    private static long threshold(long k, long bias) {
        long value = k - bias;
        if (value < P_TMIN) return P_TMIN;
        if (value > P_TMAX) return P_TMAX;
        return value;
    }

    private static int basicDigit(int c) {
        if (c >= '0' && c <= '9') return 26 + c - '0';
        if (c >= 'a' && c <= 'z') return c - 'a';
        if (c >= 'A' && c <= 'Z') return c - 'A';
        return -1;
    }

    private static int digitChar(long digit) {
        return (int) (digit < 26 ? 'a' + digit : '0' + digit - 26);
    }
}
