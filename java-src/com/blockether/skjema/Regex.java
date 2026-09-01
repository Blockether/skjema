package com.blockether.skjema;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * ECMAScript regular expressions in the dialect java.util.regex speaks.
 *
 * <p>{@code pattern} and {@code patternProperties} hold ECMA-262 regular
 * expressions, and Java's are ALMOST the same language. Every difference
 * between the two is silent - the pattern compiles on both sides and matches
 * different strings:
 *
 * <ul>
 * <li>{@code \s} is six ASCII characters in Java; in ECMAScript it is every
 *     space separator, both line terminators and the byte-order mark,
 * <li>{@code \v} is one vertical tab in ECMAScript and a whole class of
 *     vertical whitespace in Java,
 * <li>{@code \ca} and {@code \cA} are the same control character in
 *     ECMAScript, which takes the letter modulo 32; Java exclusive-ors with 64
 *     and answers {@code !},
 * <li>{@code \b} inside a character class is a backspace in ECMAScript,
 * <li>{@code \0} is NUL in ECMAScript and the start of an octal escape in Java,
 * <li>{@code \p{Letter}} and {@code \p{Script=Greek}} are {@code \p{L}} and
 *     {@code \p{IsGreek}}.
 * </ul>
 *
 * <p>One scan answers both questions this library asks of a pattern: how Java
 * spells it, and whether it was ECMAScript in the first place - which is what
 * {@code format: regex} asserts, and why {@code (?P<name>...)},
 * {@code (?#comment)} and the inline flags {@code (?i)} are refused even though
 * Java understands two of them.
 */
public final class Regex {
    private Regex() {}

    /** How the scan came out: the Java spelling, and why it was not ECMAScript. */
    private record Scan(String java, String error) {}

    /**
     * What ECMAScript's {@code \s} matches - WhiteSpace, LineTerminator and the
     * byte-order mark - written as the body of a java.util.regex class. The
     * characters are spelled in hexadecimal because javac reads a unicode escape
     * before the string literal is a string.
     */
    private static final String ECMA_SPACE =
        "\\t\\n\\x0B\\f\\r \\x{a0}\\x{1680}\\x{2000}-\\x{200a}\\x{2028}\\x{2029}"
        + "\\x{202f}\\x{205f}\\x{3000}\\x{feff}";

    /**
     * The long general-category names ECMAScript writes in {@code \p{...}} and
     * the short ones java.util.regex answers to. Java knows {@code \p{L}}; it
     * has never heard of {@code \p{Letter}}.
     */
    private static final Map<String, String> GENERAL_CATEGORIES = Map.ofEntries(
        Map.entry("Letter", "L"), Map.entry("Lowercase_Letter", "Ll"),
        Map.entry("Uppercase_Letter", "Lu"), Map.entry("Titlecase_Letter", "Lt"),
        Map.entry("Modifier_Letter", "Lm"), Map.entry("Other_Letter", "Lo"),
        Map.entry("Cased_Letter", "LC"),
        Map.entry("Mark", "M"), Map.entry("Nonspacing_Mark", "Mn"),
        Map.entry("Spacing_Mark", "Mc"), Map.entry("Enclosing_Mark", "Me"),
        Map.entry("Number", "N"), Map.entry("Decimal_Number", "Nd"),
        Map.entry("Letter_Number", "Nl"), Map.entry("Other_Number", "No"),
        Map.entry("Punctuation", "P"), Map.entry("Connector_Punctuation", "Pc"),
        Map.entry("Dash_Punctuation", "Pd"), Map.entry("Open_Punctuation", "Ps"),
        Map.entry("Close_Punctuation", "Pe"), Map.entry("Initial_Punctuation", "Pi"),
        Map.entry("Final_Punctuation", "Pf"), Map.entry("Other_Punctuation", "Po"),
        Map.entry("Symbol", "S"), Map.entry("Math_Symbol", "Sm"),
        Map.entry("Currency_Symbol", "Sc"), Map.entry("Modifier_Symbol", "Sk"),
        Map.entry("Other_Symbol", "So"),
        Map.entry("Separator", "Z"), Map.entry("Space_Separator", "Zs"),
        Map.entry("Line_Separator", "Zl"), Map.entry("Paragraph_Separator", "Zp"),
        Map.entry("Other", "C"), Map.entry("Control", "Cc"), Map.entry("Format", "Cf"),
        Map.entry("Surrogate", "Cs"), Map.entry("Private_Use", "Co"),
        Map.entry("Unassigned", "Cn"));

    /**
     * `p` translated and compiled, kept: `patternProperties` matches the same
     * handful of patterns against every property name of every instance.
     */
    private static final ConcurrentHashMap<String, Pattern> COMPILED = new ConcurrentHashMap<>();

    /** `p` translated and compiled, from the cache when it has been asked before. */
    public static Pattern patternOf(String p) {
        Pattern cached = COMPILED.get(p);
        return cached != null ? cached : COMPILED.computeIfAbsent(p, text -> Pattern.compile(translate(text)));
    }

    /** The java.util.regex spelling of the ECMAScript pattern `p`. */
    public static String translate(String p) {
        return scan(p).java();
    }

    /**
     * Whether `p` is an ECMAScript regular expression, which is what
     * {@code format: regex} asserts. Compiling under Java is not enough on its
     * own: Java accepts {@code (?i)}, {@code \a} and refuses {@code (?#comment)}
     * for reasons of its own.
     */
    public static boolean ecma(String p) {
        Scan scan = scan(p);
        if (scan.error() != null) return false;
        try {
            Pattern.compile(scan.java());
            return true;
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    private static int at(String p, int i) {
        return i >= 0 && i < p.length() ? p.charAt(i) : -1;
    }

    private static boolean isDigit(int c) {
        return c >= '0' && c <= '9';
    }

    private static boolean isLetter(int c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    private static boolean isHex(int c) {
        return isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }

    /**
     * Every letter ECMAScript gives a meaning after a backslash. A letter
     * outside this set is not an escape at all - {@code \a} is a syntax error,
     * not a bell.
     */
    private static boolean isEscapeLetter(int c) {
        return "dDwWsSbBfnrtvcxukpP".indexOf(c) >= 0;
    }

    /**
     * One {@code \p{...}} body in java.util.regex spelling. A general category
     * keeps its short name, a script or binary property takes Java's {@code Is}
     * prefix.
     */
    private static String unicodeProperty(String body) {
        String trimmed = body.trim();
        int equals = trimmed.indexOf('=');
        String key = equals < 0 ? null : trimmed.substring(0, equals).trim();
        String value = equals < 0 ? trimmed : trimmed.substring(equals + 1).trim();
        if (key == null) {
            String category = GENERAL_CATEGORIES.get(value);
            if (category != null) return category;
            return isShortCategory(value) ? value : "Is" + value;
        }
        if (key.equals("General_Category") || key.equals("gc")) {
            String category = GENERAL_CATEGORIES.get(value);
            return category != null ? category : value;
        }
        return "Is" + value;
    }

    private static boolean isShortCategory(String value) {
        if (value.isEmpty() || value.length() > 2) return false;
        if ("LMNPSZC".indexOf(value.charAt(0)) < 0) return false;
        return value.length() == 1 || isLetter(value.charAt(1));
    }

    /**
     * Read `p` once and answer its Java spelling together with the first reason
     * it was not an ECMAScript pattern.
     *
     * <p>The spelling is always the closest java.util.regex one, so a pattern
     * Java accepts still compiles even when ECMAScript would have refused it;
     * the reason is the whole judgement {@code format: regex} makes.
     */
    private static Scan scan(String p) {
        int n = p.length();
        StringBuilder out = new StringBuilder(n + 16);
        String error = null;
        boolean inClass = false;
        int i = 0;
        while (i < n) {
            char c = p.charAt(i);
            if (c == '\\') {
                int d = at(p, i + 1);
                if (d < 0) {
                    out.append(c);
                    error = error != null ? error : "the pattern ends in a backslash";
                    i += 1;
                } else if (d == 's') {
                    out.append(inClass ? ECMA_SPACE : "[" + ECMA_SPACE + "]");
                    i += 2;
                } else if (d == 'S') {
                    out.append("[^").append(ECMA_SPACE).append(']');
                    i += 2;
                } else if (d == 'v') {
                    out.append("\\x0B");
                    i += 2;
                } else if (d == 'b' && inClass) {
                    out.append("\\x08");
                    i += 2;
                } else if (d == '0' && !isDigit(at(p, i + 2))) {
                    out.append("\\x00");
                    i += 2;
                } else if (d == 'c') {
                    int letter = at(p, i + 2);
                    if (isLetter(letter)) {
                        out.append("\\c").append(Character.toUpperCase((char) letter));
                        i += 3;
                    } else {
                        out.append("\\c");
                        error = error != null ? error : "a control escape needs a letter after it";
                        i += 2;
                    }
                } else if (d == 'p' || d == 'P') {
                    int close = at(p, i + 2) == '{' ? p.indexOf('}', i + 3) : -1;
                    if (close > 0) {
                        out.append('\\').append((char) d).append('{')
                           .append(unicodeProperty(p.substring(i + 3, close))).append('}');
                        i = close + 1;
                    } else {
                        out.append('\\').append((char) d);
                        error = error != null ? error : "a property escape needs a {name} after it";
                        i += 2;
                    }
                } else if (d == 'x') {
                    if (isHex(at(p, i + 2)) && isHex(at(p, i + 3))) {
                        out.append("\\x").append(p, i + 2, i + 4);
                        i += 4;
                    } else {
                        out.append("\\x");
                        error = error != null ? error : "a hexadecimal escape needs two digits";
                        i += 2;
                    }
                } else if (d == 'u') {
                    int close = at(p, i + 2) == '{' ? p.indexOf('}', i + 3) : -1;
                    if (close > 0) {
                        out.append("\\x{").append(p, i + 3, close).append('}');
                        i = close + 1;
                    } else if (isHex(at(p, i + 2)) && isHex(at(p, i + 3))
                            && isHex(at(p, i + 4)) && isHex(at(p, i + 5))) {
                        out.append('\\').append('u').append(p, i + 2, i + 6);
                        i += 6;
                    } else {
                        out.append('\\').append('u');
                        error = error != null ? error
                            : "a unicode escape needs four digits or {digits}";
                        i += 2;
                    }
                } else if (d == 'k') {
                    if (at(p, i + 2) == '<') {
                        out.append("\\k<");
                        i += 3;
                    } else {
                        out.append("\\k");
                        error = error != null ? error : "a back reference needs a <name> after it";
                        i += 2;
                    }
                } else if (isEscapeLetter(d) || isDigit(d)) {
                    out.append('\\').append((char) d);
                    i += 2;
                } else if (isLetter(d)) {
                    out.append('\\').append((char) d);
                    error = error != null ? error
                        : "`\\" + (char) d + "` is not an ECMAScript escape";
                    i += 2;
                } else {
                    out.append('\\').append((char) d);
                    i += 2;
                }
                continue;
            }

            // ECMAScript has an empty class, which matches nothing, and its
            // negation, which matches anything; java.util.regex has neither.
            if (c == '[' && !inClass && at(p, i + 1) == ']') {
                out.append("[^\\x{0}-\\x{10FFFF}]");
                i += 2;
            } else if (c == '[' && !inClass && at(p, i + 1) == '^' && at(p, i + 2) == ']') {
                out.append("[\\x{0}-\\x{10FFFF}]");
                i += 3;
            } else if (c == '[' && !inClass) {
                out.append(c);
                inClass = true;
                i += 1;
            } else if (c == ']' && inClass) {
                out.append(c);
                inClass = false;
                i += 1;
            } else if (c == '(' && !inClass && at(p, i + 1) == '?') {
                int kind = at(p, i + 2);
                if (kind == ':' || kind == '=' || kind == '!') {
                    out.append("(?").append((char) kind);
                    i += 3;
                } else if (kind == '<') {
                    int look = at(p, i + 3);
                    if (look == '=' || look == '!') {
                        out.append("(?<").append((char) look);
                        i += 4;
                    } else {
                        out.append("(?<");
                        i += 3;
                    }
                } else {
                    out.append("(?");
                    error = error != null ? error
                        : "`(?" + (kind < 0 ? "" : String.valueOf((char) kind))
                          + "` is not an ECMAScript group";
                    i += 2;
                }
            } else {
                out.append(c);
                i += 1;
            }
        }
        if (error == null && inClass) error = "the character class is never closed";
        return new Scan(out.toString(), error);
    }
}
