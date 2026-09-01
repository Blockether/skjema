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

/** The compiled validator, plus the allocation-sensitive IDN loops. */
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

    private static final Node ANY = value -> true;
    private static final Node NONE = value -> false;
    private static final Object MISSING = new Object();

    /**
     * Compile a schema into the checks it declares and nothing else: a node exists
     * only because a keyword asked for it, so a small schema costs a small call.
     * An unsupported keyword aborts the whole compilation and the caller falls back
     * to the complete evaluator.
     */
    private static Node compileNode(Object raw, IFn patternCompiler) {
        if (Boolean.TRUE.equals(raw)) return ANY;
        if (Boolean.FALSE.equals(raw)) return NONE;
        if (!(raw instanceof Map<?, ?> source)) throw new UnsupportedSchema();
        if (source.get("$schema") instanceof String dialect && !DIALECTS.contains(dialect)) throw new UnsupportedSchema();
        for (String keyword : ADVANCED) if (source.containsKey(keyword)) throw new UnsupportedSchema();
        if (source.get("items") instanceof List<?>) throw new UnsupportedSchema();

        ArrayList<Node> checks = new ArrayList<>();
        addType(checks, source.get("type"));
        addValues(checks, source);
        addNumbers(checks, source);
        addStrings(checks, source, patternCompiler);
        addArrays(checks, source, patternCompiler);
        addObjects(checks, source, patternCompiler);
        return all(checks);
    }

    private static Node all(List<Node> checks) {
        if (checks.isEmpty()) return ANY;
        if (checks.size() == 1) return checks.get(0);
        if (checks.size() == 2) {
            Node first = checks.get(0);
            Node second = checks.get(1);
            return value -> first.valid(value) && second.valid(value);
        }
        Node[] nodes = checks.toArray(new Node[0]);
        return value -> {
            for (Node node : nodes) if (!node.valid(value)) return false;
            return true;
        };
    }

    private static void addType(List<Node> checks, Object declared) {
        if (declared == null) return;
        String[] types = strings(declared);
        if (types.length == 1) {
            checks.add(typeNode(types[0]));
            return;
        }
        Node[] nodes = new Node[types.length];
        for (int i = 0; i < types.length; i++) nodes[i] = typeNode(types[i]);
        checks.add(value -> {
            for (Node node : nodes) if (node.valid(value)) return true;
            return false;
        });
    }

    private static Node typeNode(String type) {
        return switch (type) {
            case "null" -> value -> value == null;
            case "boolean" -> value -> value instanceof Boolean;
            case "object" -> value -> value instanceof Map<?, ?>;
            case "array" -> value -> list(value) != null;
            case "number" -> Fast::jsonNumber;
            case "integer" -> value -> jsonNumber(value) && integral((Number) value);
            case "string" -> value -> value instanceof String;
            default -> throw new UnsupportedSchema();
        };
    }

    private static void addValues(List<Node> checks, Map<?, ?> source) {
        Object declared = source.get("enum");
        if (declared != null) {
            if (!(declared instanceof List<?> values)) throw new UnsupportedSchema();
            checks.add(enumNode(values));
        }
        if (source.containsKey("const")) {
            Object expected = canonical(source.get("const"));
            checks.add(value -> expected == null ? value == null : expected.equals(canonical(value)));
        }
    }

    /** A string-only enum answers from a set; anything else compares canonical JSON values. */
    private static Node enumNode(List<?> values) {
        boolean strings = !values.isEmpty();
        for (Object value : values) {
            if (!(value instanceof String)) {
                strings = false;
                break;
            }
        }
        if (strings) {
            HashSet<Object> allowed = new HashSet<>(values);
            return value -> value instanceof String && allowed.contains(value);
        }
        Object[] allowed = new Object[values.size()];
        for (int i = 0; i < values.size(); i++) allowed[i] = canonical(values.get(i));
        return value -> {
            Object actual = canonical(value);
            for (Object expected : allowed) {
                if (expected == null ? actual == null : expected.equals(actual)) return true;
            }
            return false;
        };
    }

    private static void addNumbers(List<Node> checks, Map<?, ?> source) {
        Number multipleOf = number(source.get("multipleOf"));
        if (multipleOf != null) checks.add(new MultipleOf(multipleOf));
        Number maximum = number(source.get("maximum"));
        if (maximum != null) checks.add(new Bound(maximum, true, false));
        Number exclusiveMaximum = number(source.get("exclusiveMaximum"));
        if (exclusiveMaximum != null) checks.add(new Bound(exclusiveMaximum, true, true));
        Number minimum = number(source.get("minimum"));
        if (minimum != null) checks.add(new Bound(minimum, false, false));
        Number exclusiveMinimum = number(source.get("exclusiveMinimum"));
        if (exclusiveMinimum != null) checks.add(new Bound(exclusiveMinimum, false, true));
    }

    /**
     * Both bounds of a string are usually settled without counting code points: a
     * code point is never more than two UTF-16 units and never fewer than one, so
     * the unit count brackets the answer.
     */
    private static void addStrings(List<Node> checks, Map<?, ?> source, IFn patternCompiler) {
        Long declaredMax = nonnegativeLong(source.get("maxLength"));
        if (declaredMax != null) {
            long max = declaredMax;
            checks.add(value -> {
                if (!(value instanceof String text)) return true;
                int units = text.length();
                if (units <= max) return true;
                if ((units + 1) / 2 > max) return false;
                return text.codePointCount(0, units) <= max;
            });
        }
        Long declaredMin = nonnegativeLong(source.get("minLength"));
        if (declaredMin != null) {
            long min = declaredMin;
            checks.add(value -> {
                if (!(value instanceof String text)) return true;
                int units = text.length();
                if (units < min) return false;
                if ((units + 1) / 2 >= min) return true;
                return text.codePointCount(0, units) >= min;
            });
        }
        if (source.get("pattern") instanceof String text) {
            Pattern pattern = (Pattern) patternCompiler.invoke(text);
            checks.add(value -> !(value instanceof String actual) || pattern.matcher(actual).find());
        }
    }

    private static void addArrays(List<Node> checks, Map<?, ?> source, IFn patternCompiler) {
        Long maxItems = nonnegativeLong(source.get("maxItems"));
        Long minItems = nonnegativeLong(source.get("minItems"));
        boolean unique = Boolean.TRUE.equals(source.get("uniqueItems"));
        List<Node> prefixItems = compileNodes(source.get("prefixItems"), patternCompiler);
        Node items = schemaNode(source.get("items"), patternCompiler);
        if (maxItems == null && minItems == null && !unique && prefixItems == null && items == null) return;
        checks.add(new ArrayNode(
                minItems == null ? -1L : minItems,
                maxItems == null ? -1L : maxItems,
                unique,
                prefixItems == null ? null : prefixItems.toArray(new Node[0]),
                items));
    }

    private static void addObjects(List<Node> checks, Map<?, ?> source, IFn patternCompiler) {
        Long maxProperties = nonnegativeLong(source.get("maxProperties"));
        Long minProperties = nonnegativeLong(source.get("minProperties"));
        String[] required = strings(source.get("required"));
        Map<Object, Node> properties = compileProperties(source.get("properties"), patternCompiler);
        Map<Object, String[]> dependentRequired = compileRequired(source.get("dependentRequired"));
        boolean hasAdditional = source.containsKey("additionalProperties");
        if (maxProperties == null && minProperties == null && required == null
                && properties == null && dependentRequired == null && !hasAdditional) {
            return;
        }
        checks.add(new ObjectNode(
                minProperties == null ? -1L : minProperties,
                maxProperties == null ? -1L : maxProperties,
                required,
                properties,
                dependentRequired,
                hasAdditional,
                schemaNode(source.get("additionalProperties"), patternCompiler)));
    }

    /** One numeric bound. An integral bound answers integral instances in primitives. */
    private static final class Bound implements Node {
        private final Number bound;
        private final long integral;
        private final boolean isIntegral;
        private final boolean upper;
        private final boolean exclusive;

        Bound(Number bound, boolean upper, boolean exclusive) {
            this.bound = bound;
            this.upper = upper;
            this.exclusive = exclusive;
            this.isIntegral = fitsLong(bound);
            this.integral = this.isIntegral ? bound.longValue() : 0L;
        }

        @Override
        public boolean valid(Object value) {
            if (isIntegral) {
                if (value instanceof Long actual) return accept(Long.compare(actual, integral));
                if (value instanceof Integer actual) return accept(Long.compare(actual, integral));
            }
            if (!(value instanceof Number actual) || !jsonNumber(actual)) return true;
            return accept(compare(actual, bound));
        }

        private boolean accept(int sign) {
            if (upper) return exclusive ? sign < 0 : sign <= 0;
            return exclusive ? sign > 0 : sign >= 0;
        }
    }

    private static final class MultipleOf implements Node {
        private final Number divisor;
        private final long integral;
        private final boolean isIntegral;

        MultipleOf(Number divisor) {
            this.divisor = divisor;
            this.isIntegral = fitsLong(divisor) && divisor.longValue() != 0L;
            this.integral = this.isIntegral ? divisor.longValue() : 0L;
        }

        @Override
        public boolean valid(Object value) {
            if (isIntegral) {
                if (value instanceof Long actual) return actual % integral == 0L;
                if (value instanceof Integer actual) return actual % integral == 0L;
            }
            if (!(value instanceof Number actual) || !jsonNumber(actual)) return true;
            return multipleOf(actual, divisor);
        }
    }

    /** Every array keyword the schema declared, over one look at the instance. */
    private static final class ArrayNode implements Node {
        private final long min;
        private final long max;
        private final boolean unique;
        private final Node[] prefixItems;
        private final Node items;

        ArrayNode(long min, long max, boolean unique, Node[] prefixItems, Node items) {
            this.min = min;
            this.max = max;
            this.unique = unique;
            this.prefixItems = prefixItems;
            this.items = items;
        }

        @Override
        public boolean valid(Object value) {
            List<?> array = list(value);
            if (array == null) return true;
            int size = array.size();
            if (max >= 0 && size > max) return false;
            if (min >= 0 && size < min) return false;
            if (unique && hasDuplicate(array)) return false;
            int prefixed = prefixItems == null ? 0 : Math.min(prefixItems.length, size);
            for (int i = 0; i < prefixed; i++) if (!prefixItems[i].valid(array.get(i))) return false;
            if (items != null) for (int i = prefixed; i < size; i++) if (!items.valid(array.get(i))) return false;
            return true;
        }
    }

    /** Every object keyword the schema declared, one map lookup per property. */
    private static final class ObjectNode implements Node {
        private final long min;
        private final long max;
        private final String[] required;
        private final boolean[] mandatory;
        private final Object[] names;
        private final Node[] nodes;
        private final Set<Object> declared;
        private final Object[] dependentOn;
        private final String[][] dependentNames;
        private final boolean hasAdditional;
        private final Node additional;

        ObjectNode(long min, long max, String[] required, Map<Object, Node> properties,
                   Map<Object, String[]> dependentRequired, boolean hasAdditional, Node additional) {
            this.min = min;
            this.max = max;
            this.hasAdditional = hasAdditional;
            this.additional = additional;
            int count = properties == null ? 0 : properties.size();
            this.names = count == 0 ? null : new Object[count];
            this.nodes = count == 0 ? null : new Node[count];
            this.mandatory = count == 0 ? null : new boolean[count];
            if (count > 0) {
                int i = 0;
                for (Map.Entry<Object, Node> entry : properties.entrySet()) {
                    names[i] = entry.getKey();
                    nodes[i] = entry.getValue();
                    i++;
                }
            }
            // A required name that `properties` also declares is answered by the one
            // lookup that check already makes, so only the names without a declared
            // property are left to a pass of their own.
            String[] outstanding = required;
            if (required != null && count > 0) {
                boolean[] covered = new boolean[required.length];
                int rest = required.length;
                for (int i = 0; i < names.length; i++) {
                    for (int r = 0; r < required.length; r++) {
                        if (!covered[r] && required[r].equals(names[i])) {
                            covered[r] = true;
                            mandatory[i] = true;
                            rest--;
                        }
                    }
                }
                outstanding = rest == 0 ? null : new String[rest];
                for (int r = 0, j = 0; outstanding != null && r < required.length; r++) {
                    if (!covered[r]) outstanding[j++] = required[r];
                }
            }
            this.required = outstanding;
            this.declared = hasAdditional && properties != null ? new HashSet<>(properties.keySet()) : null;
            int dependents = dependentRequired == null ? 0 : dependentRequired.size();
            this.dependentOn = dependents == 0 ? null : new Object[dependents];
            this.dependentNames = dependents == 0 ? null : new String[dependents][];
            if (dependents > 0) {
                int i = 0;
                for (Map.Entry<Object, String[]> entry : dependentRequired.entrySet()) {
                    dependentOn[i] = entry.getKey();
                    dependentNames[i] = entry.getValue();
                    i++;
                }
            }
        }

        @Override
        public boolean valid(Object value) {
            if (!(value instanceof Map<?, ?> object)) return true;
            if (max >= 0 && object.size() > max) return false;
            if (min >= 0 && object.size() < min) return false;
            if (required != null) for (String name : required) if (!object.containsKey(name)) return false;
            if (dependentOn != null) {
                for (int i = 0; i < dependentOn.length; i++) {
                    if (object.containsKey(dependentOn[i])) {
                        for (String name : dependentNames[i]) if (!object.containsKey(name)) return false;
                    }
                }
            }
            if (names != null) {
                for (int i = 0; i < names.length; i++) {
                    Object member = lookup(object, names[i]);
                    if (member == MISSING) {
                        if (mandatory[i]) return false;
                    } else if (!nodes[i].valid(member)) return false;
                }
            }
            if (hasAdditional) {
                for (Map.Entry<?, ?> entry : object.entrySet()) {
                    if (declared == null || !declared.contains(entry.getKey())) {
                        if (additional == null || !additional.valid(entry.getValue())) return false;
                    }
                }
            }
            return true;
        }
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

    /** One lookup that still tells a JSON `null` member apart from a missing one. */
    private static Object lookup(Map<?, ?> object, Object key) {
        if (object instanceof clojure.lang.ILookup source) return source.valAt(key, MISSING);
        Object value = object.get(key);
        return value == null && !object.containsKey(key) ? MISSING : value;
    }

    private static boolean fitsLong(Number value) {
        return value instanceof Long || value instanceof Integer || value instanceof Short || value instanceof Byte;
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
