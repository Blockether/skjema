package com.blockether.skjema;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.Numbers;
import clojure.lang.RT;
import clojure.lang.Util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/** Allocation-sensitive loops used by the Clojure API. */
public final class Fast {
    private Fast() {}

    private static final int P_BASE = 36;
    private static final int P_TMIN = 1;
    private static final int P_TMAX = 26;
    private static final int P_SKEW = 38;
    private static final int P_DAMP = 700;
    private static final int P_INITIAL_BIAS = 72;
    private static final int P_INITIAL_N = 128;

    /** Build a String from the Clojure vector used by the contextual IDN rules. */
    public static String fromCodePoints(Object points) {
        StringBuilder result = new StringBuilder();
        for (ISeq seq = RT.seq(points); seq != null; seq = seq.next()) {
            result.appendCodePoint(((Number) seq.first()).intValue());
        }
        return result.toString();
    }

    /** RFC 1123's allocation-free ASCII hostname path; false asks Clojure to take the IDN path. */
    public static boolean plainAsciiHostname(String value) {
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
                    if (delta == Long.MAX_VALUE) throw new IllegalArgumentException("punycode input is too large");
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

    /**
     * Compile the common assertion/applicator subset into a Java predicate.
     * Null means that an advanced keyword needs the complete Clojure evaluator.
     */
    public static Predicate<Object> compileValidator(Object schema, IFn patternCompiler) {
        try {
            Node node = compileNode(schema, patternCompiler);
            return node::valid;
        } catch (UnsupportedSchema ignored) {
            return null;
        }
    }

    private interface Node {
        boolean valid(Object value);
    }

    private static final class UnsupportedSchema extends RuntimeException {
        private static final long serialVersionUID = 1L;
    }

    private static final Set<String> DIALECTS = Set.of(
            "https://json-schema.org/draft/2020-12/schema");

    private static final Set<String> ADVANCED = Set.of(
            "$ref", "$dynamicRef", "$recursiveRef", "$vocabulary",
            "allOf", "anyOf", "oneOf", "not", "if", "then", "else",
            "dependentSchemas", "dependencies", "patternProperties", "propertyNames",
            "contains", "minContains", "maxContains", "unevaluatedProperties",
            "unevaluatedItems", "contentSchema");

    @SuppressWarnings("unchecked")
    private static Node compileNode(Object raw, IFn patternCompiler) {
        if (Boolean.TRUE.equals(raw)) return value -> true;
        if (Boolean.FALSE.equals(raw)) return value -> false;
        if (!(raw instanceof Map<?, ?> source)) throw new UnsupportedSchema();
        Object dialect = source.get("$schema");
        if (dialect instanceof String text && !DIALECTS.contains(text)) throw new UnsupportedSchema();
        for (String keyword : ADVANCED) if (source.containsKey(keyword)) throw new UnsupportedSchema();
        if (source.get("items") instanceof List<?>) throw new UnsupportedSchema();

        Object type = source.get("type");
        String[] types = strings(type);
        List<Object> enumValues = source.get("enum") instanceof List<?> values
                ? new ArrayList<>((List<Object>) values) : null;
        boolean hasConst = source.containsKey("const");
        Object constValue = source.get("const");

        Number multipleOf = number(source.get("multipleOf"));
        Number maximum = number(source.get("maximum"));
        Number exclusiveMaximum = number(source.get("exclusiveMaximum"));
        Number minimum = number(source.get("minimum"));
        Number exclusiveMinimum = number(source.get("exclusiveMinimum"));

        Long maxLength = nonnegativeLong(source.get("maxLength"));
        Long minLength = nonnegativeLong(source.get("minLength"));
        Pattern pattern = source.get("pattern") instanceof String text
                ? (Pattern) patternCompiler.invoke(text) : null;

        Long maxItems = nonnegativeLong(source.get("maxItems"));
        Long minItems = nonnegativeLong(source.get("minItems"));
        boolean uniqueItems = Boolean.TRUE.equals(source.get("uniqueItems"));
        Node items = schemaNode(source.get("items"), patternCompiler);
        List<Node> prefixItems = compileNodes(source.get("prefixItems"), patternCompiler);

        Long maxProperties = nonnegativeLong(source.get("maxProperties"));
        Long minProperties = nonnegativeLong(source.get("minProperties"));
        Map<Object, Node> properties = compileProperties(source.get("properties"), patternCompiler);
        String[] required = strings(source.get("required"));
        Map<Object, String[]> dependentRequired = compileRequired(source.get("dependentRequired"));
        boolean hasAdditional = source.containsKey("additionalProperties");
        Node additional = schemaNode(source.get("additionalProperties"), patternCompiler);

        return value -> {
            if (types != null && !matchesAnyType(types, value)) return false;
            if (enumValues != null && enumValues.stream().noneMatch(expected -> jsonEqual(expected, value))) return false;
            if (hasConst && !jsonEqual(constValue, value)) return false;

            if (jsonNumber(value)) {
                Number actual = (Number) value;
                if (multipleOf != null && !multipleOf(actual, multipleOf)) return false;
                if (maximum != null && compare(actual, maximum) > 0) return false;
                if (exclusiveMaximum != null && compare(actual, exclusiveMaximum) >= 0) return false;
                if (minimum != null && compare(actual, minimum) < 0) return false;
                if (exclusiveMinimum != null && compare(actual, exclusiveMinimum) <= 0) return false;
            }
            if (value instanceof String text) {
                int length = text.codePointCount(0, text.length());
                if (maxLength != null && length > maxLength) return false;
                if (minLength != null && length < minLength) return false;
                if (pattern != null && !pattern.matcher(text).find()) return false;
            }

            List<?> array = list(value);
            if (array != null) {
                int size = array.size();
                if (maxItems != null && size > maxItems) return false;
                if (minItems != null && size < minItems) return false;
                if (uniqueItems && hasDuplicate(array)) return false;
                int prefixed = prefixItems == null ? 0 : Math.min(prefixItems.size(), size);
                for (int i = 0; i < prefixed; i++) if (!prefixItems.get(i).valid(array.get(i))) return false;
                if (items != null) for (int i = prefixed; i < size; i++) if (!items.valid(array.get(i))) return false;
            }

            if (value instanceof Map<?, ?> object) {
                int size = object.size();
                if (maxProperties != null && size > maxProperties) return false;
                if (minProperties != null && size < minProperties) return false;
                if (required != null) for (String name : required) if (!object.containsKey(name)) return false;
                if (dependentRequired != null) {
                    for (Map.Entry<Object, String[]> entry : dependentRequired.entrySet()) {
                        if (object.containsKey(entry.getKey())) {
                            for (String name : entry.getValue()) if (!object.containsKey(name)) return false;
                        }
                    }
                }
                if (properties != null) {
                    for (Map.Entry<Object, Node> entry : properties.entrySet()) {
                        if (object.containsKey(entry.getKey()) && !entry.getValue().valid(object.get(entry.getKey()))) return false;
                    }
                }
                if (hasAdditional) {
                    for (Map.Entry<?, ?> entry : object.entrySet()) {
                        if (properties == null || !properties.containsKey(entry.getKey())) {
                            if (additional == null || !additional.valid(entry.getValue())) return false;
                        }
                    }
                }
            }
            return true;
        };
    }

    private static Node schemaNode(Object value, IFn patternCompiler) {
        if (value == null) return null;
        return compileNode(value, patternCompiler);
    }

    private static List<Node> compileNodes(Object value, IFn patternCompiler) {
        List<?> values = list(value);
        if (values == null) return null;
        ArrayList<Node> result = new ArrayList<>(values.size());
        for (Object item : values) result.add(compileNode(item, patternCompiler));
        return result;
    }

    private static Map<Object, Node> compileProperties(Object value, IFn patternCompiler) {
        if (!(value instanceof Map<?, ?> values)) return null;
        HashMap<Object, Node> result = new HashMap<>(values.size() * 2);
        for (Map.Entry<?, ?> entry : values.entrySet()) result.put(entry.getKey(), compileNode(entry.getValue(), patternCompiler));
        return result;
    }

    private static Map<Object, String[]> compileRequired(Object value) {
        if (!(value instanceof Map<?, ?> values)) return null;
        HashMap<Object, String[]> result = new HashMap<>(values.size() * 2);
        for (Map.Entry<?, ?> entry : values.entrySet()) {
            String[] names = strings(entry.getValue());
            if (names == null) throw new UnsupportedSchema();
            result.put(entry.getKey(), names);
        }
        return result;
    }

    private static String[] strings(Object value) {
        if (value == null) return null;
        if (value instanceof String text) return new String[]{text};
        List<?> values = list(value);
        if (values == null) throw new UnsupportedSchema();
        String[] result = new String[values.size()];
        for (int i = 0; i < values.size(); i++) {
            if (!(values.get(i) instanceof String text)) throw new UnsupportedSchema();
            result[i] = text;
        }
        return result;
    }

    private static Number number(Object value) {
        return value instanceof Number number ? number : null;
    }

    private static Long nonnegativeLong(Object value) {
        if (!(value instanceof Number number)) return null;
        long result = number.longValue();
        return result >= 0 ? result : null;
    }

    private static boolean matchesAnyType(String[] types, Object value) {
        for (String type : types) if (matchesType(type, value)) return true;
        return false;
    }

    private static boolean matchesType(String type, Object value) {
        return switch (type) {
            case "null" -> value == null;
            case "boolean" -> value instanceof Boolean;
            case "object" -> value instanceof Map<?, ?>;
            case "array" -> list(value) != null;
            case "number" -> jsonNumber(value);
            case "integer" -> jsonNumber(value) && integral((Number) value);
            case "string" -> value instanceof String;
            default -> true;
        };
    }

    private static List<?> list(Object value) {
        if (value instanceof List<?> values) return values;
        if (!(value instanceof clojure.lang.Sequential)) return null;
        ArrayList<Object> result = new ArrayList<>();
        for (ISeq seq = RT.seq(value); seq != null; seq = seq.next()) result.add(seq.first());
        return result;
    }

    private static boolean jsonNumber(Object value) {
        if (!(value instanceof Number number)) return false;
        if (number instanceof Double d) return Double.isFinite(d);
        if (number instanceof Float f) return Float.isFinite(f);
        return true;
    }

    private static boolean integral(Number value) {
        if (value instanceof Double d) return Double.isFinite(d) && d == Math.rint(d);
        if (value instanceof Float f) return Float.isFinite(f) && f == Math.rint(f);
        if (value instanceof BigDecimal decimal) return decimal.stripTrailingZeros().scale() <= 0;
        return value instanceof Byte || value instanceof Short || value instanceof Integer
                || value instanceof Long || value instanceof BigInteger || value instanceof clojure.lang.BigInt;
    }

    private static BigDecimal decimal(Number value) {
        if (value instanceof BigDecimal decimal) return decimal;
        if (value instanceof BigInteger integer) return new BigDecimal(integer);
        if (value instanceof clojure.lang.BigInt integer) return new BigDecimal(integer.toBigInteger());
        if (value instanceof clojure.lang.Ratio ratio) {
            return new BigDecimal(ratio.numerator).divide(new BigDecimal(ratio.denominator), MathContext.DECIMAL128);
        }
        if (value instanceof Double || value instanceof Float) return BigDecimal.valueOf(value.doubleValue());
        return BigDecimal.valueOf(value.longValue());
    }

    private static int compare(Number left, Number right) {
        return decimal(left).compareTo(decimal(right));
    }

    private static boolean multipleOf(Number value, Number divisor) {
        try {
            BigDecimal d = decimal(divisor);
            return d.signum() != 0 && decimal(value).remainder(d).signum() == 0;
        } catch (ArithmeticException ignored) {
            return false;
        }
    }

    private static final class NumberKey {
        private final Number value;

        NumberKey(Number value) { this.value = value; }

        @Override
        public boolean equals(Object other) {
            return other instanceof NumberKey key && Numbers.equiv(value, key.value);
        }

        @Override
        public int hashCode() { return Util.hasheq(value); }
    }

    private static Object canonical(Object value) {
        if (value instanceof Number number && jsonNumber(number)) return new NumberKey(number);
        if (value instanceof Map<?, ?> map) {
            HashMap<Object, Object> result = new HashMap<>(map.size() * 2);
            for (Map.Entry<?, ?> entry : map.entrySet()) result.put(entry.getKey(), canonical(entry.getValue()));
            return result;
        }
        List<?> values = list(value);
        if (values != null) {
            ArrayList<Object> result = new ArrayList<>(values.size());
            for (Object item : values) result.add(canonical(item));
            return result;
        }
        return value;
    }

    private static boolean jsonEqual(Object left, Object right) {
        if (left == right) return true;
        if (left == null || right == null) return false;
        return canonical(left).equals(canonical(right));
    }

    private static boolean hasDuplicate(List<?> values) {
        HashSet<Object> seen = new HashSet<>(values.size() * 2);
        for (Object value : values) if (!seen.add(canonical(value))) return true;
        return false;
    }
}
