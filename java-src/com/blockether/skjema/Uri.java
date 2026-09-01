package com.blockether.skjema;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * URI reference resolution and JSON Pointer tokens - the addressing half of
 * JSON Schema.
 *
 * <p>Identifiers in a schema are URI REFERENCES: {@code $id} moves the base,
 * {@code $ref} resolves against whatever base encloses it, and a fragment is
 * either an anchor name or a JSON Pointer. Everything here is string work;
 * nothing fetches anything, because a validator that resolves a reference over
 * the network is a validator that fails differently on every machine.
 */
public final class Uri {
    private Uri() {}

    /**
     * The URI without its fragment. Null in, empty string out, because the base
     * of a schema that never declared one is the empty URI.
     */
    public static String stripFragment(String u) {
        if (u == null) return "";
        int i = u.indexOf('#');
        return i < 0 ? u : u.substring(0, i);
    }

    /**
     * The fragment of `u` WITHOUT its `#`, or null when it carries none. An
     * empty fragment ({@code uri#}) answers the empty string, which is a
     * different fact.
     */
    public static String fragment(String u) {
        if (u == null) return null;
        int i = u.indexOf('#');
        return i < 0 ? null : u.substring(i + 1);
    }

    /** True when the reference carries a scheme, so it resolves to itself. */
    public static boolean absolute(String u) {
        if (u == null) return false;
        int n = u.length();
        for (int i = 0; i < n; i++) {
            char c = u.charAt(i);
            if (c == ':') return i > 0;
            boolean alpha = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
            if (i == 0) {
                if (!alpha) return false;
            } else if (!(alpha || (c >= '0' && c <= '9') || c == '+' || c == '.' || c == '-')) {
                return false;
            }
        }
        return false;
    }

    /**
     * Resolve reference `ref` against `base`, RFC 3986 style.
     *
     * <p>The two cases a URI library gets wrong for schemas are handled first:
     * an empty reference is the base itself, and a fragment-only reference
     * replaces the base's fragment - including against an opaque base like
     * {@code urn:uuid:...}, where {@code java.net.URI.resolve} would hand the
     * fragment straight back.
     */
    public static String resolveRef(String base, String ref) {
        String from = base == null ? "" : base;
        if (ref == null || ref.isEmpty()) return from;
        if (ref.charAt(0) == '#') return stripFragment(from) + ref;
        if (absolute(ref)) return ref;
        if (from.isEmpty()) return ref;
        if (absolute(from)) {
            try {
                return URI.create(from).resolve(ref).toString();
            } catch (RuntimeException ignored) {
                return ref;
            }
        }
        return ref;
    }

    /**
     * Decode {@code %XX} escapes as UTF-8. A pointer fragment is part of a URI,
     * so {@code /foo%20bar} and {@code /foo bar} name the same member.
     */
    public static String percentDecode(String s) {
        int escape = s.indexOf('%');
        if (escape < 0) return s;
        int n = s.length();
        ByteArrayOutputStream out = new ByteArrayOutputStream(n);
        int i = 0;
        while (i < n) {
            char c = s.charAt(i);
            if (c == '%' && i + 3 <= n) {
                out.write(Integer.parseInt(s.substring(i + 1, i + 3), 16));
                i += 3;
            } else {
                byte[] utf8 = String.valueOf(c).getBytes(StandardCharsets.UTF_8);
                out.write(utf8, 0, utf8.length);
                i++;
            }
        }
        return out.toString(StandardCharsets.UTF_8);
    }

    /**
     * One JSON Pointer token as the member name it addresses: {@code ~1} is
     * {@code /} and {@code ~0} is {@code ~}, in that order, so {@code ~01}
     * decodes to {@code ~1} and not to {@code /}.
     */
    public static String unescapeToken(String t) {
        String decoded = percentDecode(t);
        int escape = decoded.indexOf('~');
        if (escape < 0) return decoded;
        StringBuilder out = new StringBuilder(decoded.length());
        out.append(decoded, 0, escape);
        for (int i = escape; i < decoded.length(); i++) {
            char c = decoded.charAt(i);
            if (c == '~' && i + 1 < decoded.length()) {
                char next = decoded.charAt(i + 1);
                if (next == '1') {
                    out.append('/');
                    i++;
                    continue;
                }
                if (next == '0') {
                    out.append('~');
                    i++;
                    continue;
                }
            }
            out.append(c);
        }
        return out.toString();
    }

    /** One member name as a JSON Pointer token. */
    public static String escapeToken(String t) {
        int n = t.length();
        int escape = -1;
        for (int i = 0; i < n; i++) {
            char c = t.charAt(i);
            if (c == '~' || c == '/') {
                escape = i;
                break;
            }
        }
        if (escape < 0) return t;
        StringBuilder out = new StringBuilder(n + 8);
        out.append(t, 0, escape);
        for (int i = escape; i < n; i++) {
            char c = t.charAt(i);
            if (c == '~') {
                out.append("~0");
            } else if (c == '/') {
                out.append("~1");
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }

    /**
     * Split a JSON Pointer into decoded tokens. The empty pointer addresses the
     * document itself and answers no tokens.
     */
    public static List<String> pointerTokens(String pointer) {
        List<String> tokens = new ArrayList<>();
        if (pointer == null || pointer.isEmpty()) return tokens;
        int from = pointer.indexOf('/');
        if (from < 0) return tokens;
        int start = from + 1;
        for (int i = start; i <= pointer.length(); i++) {
            if (i == pointer.length() || pointer.charAt(i) == '/') {
                tokens.add(unescapeToken(pointer.substring(start, i)));
                start = i + 1;
            }
        }
        return tokens;
    }
}
