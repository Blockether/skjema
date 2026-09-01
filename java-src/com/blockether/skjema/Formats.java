package com.blockether.skjema;

/**
 * The formats whose grammar is a walk over the characters.
 *
 * <p>A format is asserted per instance, so its grammar is on the hot path the
 * moment a schema asks for the assertion. Spelled in Clojure the cheap ones were
 * not cheap: a date-time cost 772 ns, because {@code re-matches} allocates a
 * vector of groups, every group is parsed back out of its own String, and a
 * rejected date was a caught exception. Walked here the same grammar allocates
 * nothing and reads every character once.
 *
 * <p>Only the grammars that ARE a character walk live here. Mail addresses,
 * URIs, IRIs and the internationalized host names stay where the specification
 * that owns them is written out - their grammar is a real regular expression and
 * they were measured at 33 to 237 ns, which is what a regular expression costs
 * when it is the right tool.
 */
public final class Formats {
    private Formats() {}

    private static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private static boolean isHex(char c) {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }

    private static boolean digits(String s, int from, int to) {
        for (int i = from; i < to; i++) {
            if (!isDigit(s.charAt(i))) return false;
        }
        return true;
    }

    private static int number(String s, int from, int to) {
        int value = 0;
        for (int i = from; i < to; i++) value = value * 10 + (s.charAt(i) - '0');
        return value;
    }

    private static boolean leapYear(int year) {
        return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    }

    private static int daysInMonth(int year, int month) {
        switch (month) {
            case 1: case 3: case 5: case 7: case 8: case 10: case 12: return 31;
            case 4: case 6: case 9: case 11: return 30;
            case 2: return leapYear(year) ? 29 : 28;
            default: return 0;
        }
    }

    private static boolean isDate(String s, int from, int to) {
        if (to - from != 10) return false;
        if (!digits(s, from, from + 4) || s.charAt(from + 4) != '-'
            || !digits(s, from + 5, from + 7) || s.charAt(from + 7) != '-'
            || !digits(s, from + 8, from + 10)) return false;
        int year = number(s, from, from + 4);
        int month = number(s, from + 5, from + 7);
        int day = number(s, from + 8, from + 10);
        return month >= 1 && month <= 12 && day >= 1 && day <= daysInMonth(year, month);
    }

    /**
     * RFC 3339 full-time. The offset is not optional, and a leap second is only a
     * second of the day if it is the LAST one - 23:59:60 in UTC, whatever the
     * offset spells it as locally.
     */
    private static boolean isTime(String s, int from, int to) {
        if (to - from < 9) return false;
        if (!digits(s, from, from + 2) || s.charAt(from + 2) != ':'
            || !digits(s, from + 3, from + 5) || s.charAt(from + 5) != ':'
            || !digits(s, from + 6, from + 8)) return false;
        int hour = number(s, from, from + 2);
        int minute = number(s, from + 3, from + 5);
        int second = number(s, from + 6, from + 8);
        if (hour > 23 || minute > 59) return false;

        int i = from + 8;
        if (i < to && s.charAt(i) == '.') {
            int digit = i + 1;
            while (digit < to && isDigit(s.charAt(digit))) digit++;
            if (digit == i + 1) return false;
            i = digit;
        }

        int offset;
        char zone = i < to ? s.charAt(i) : 0;
        if (zone == 'Z' || zone == 'z') {
            if (i + 1 != to) return false;
            offset = 0;
        } else if (zone == '+' || zone == '-') {
            if (to - i != 6 || !digits(s, i + 1, i + 3) || s.charAt(i + 3) != ':'
                || !digits(s, i + 4, i + 6)) return false;
            int offsetHours = number(s, i + 1, i + 3);
            int offsetMinutes = number(s, i + 4, i + 6);
            if (offsetHours > 23 || offsetMinutes > 59) return false;
            offset = (zone == '-' ? -1 : 1) * (60 * offsetHours + offsetMinutes);
        } else {
            return false;
        }

        if (second == 60) return Math.floorMod(60 * hour + minute - offset, 1440) == 1439;
        return second <= 59;
    }

    /**
     * Numbers each followed by its unit, in units ADJACENT in the order they are
     * named: a duration may start at any of them and stop at any of them, but it
     * may not skip one - `P1Y1D` says years and days without the months between.
     * Answers the index it stopped at, or -1 where it read no unit or a unit that
     * does not follow the one before it.
     */
    private static int units(String s, int from, int to, String order) {
        int i = from;
        int next = -1;
        int seen = 0;
        while (i < to && isDigit(s.charAt(i))) {
            while (i < to && isDigit(s.charAt(i))) i++;
            if (i == to) return -1;
            int at = order.indexOf(s.charAt(i));
            if (at < 0 || (next >= 0 && at != next)) return -1;
            next = at + 1;
            i++;
            seen++;
        }
        return seen == 0 ? -1 : i;
    }

    /** RFC 3339 appendix A: a date part, a time part, or a count of weeks. */
    public static boolean duration(String s) {
        int n = s.length();
        if (n < 3 || s.charAt(0) != 'P') return false;
        if (s.charAt(1) == 'T') return units(s, 2, n, "HMS") == n;

        int week = 1;
        while (week < n && isDigit(s.charAt(week))) week++;
        if (week > 1 && week + 1 == n && s.charAt(week) == 'W') return true;

        int i = units(s, 1, n, "YMD");
        if (i < 0) return false;
        if (i == n) return true;
        if (s.charAt(i) != 'T') return false;
        return units(s, i + 1, n, "HMS") == n;
    }

    /** RFC 3339 full-date, with the days the month actually has. */
    public static boolean date(String s) {
        return isDate(s, 0, s.length());
    }

    /** RFC 3339 full-time. */
    public static boolean time(String s) {
        return isTime(s, 0, s.length());
    }

    /** RFC 3339 date-time: a date, the separator, and a time. */
    public static boolean dateTime(String s) {
        int n = s.length();
        if (n <= 11) return false;
        char separator = s.charAt(10);
        return (separator == 'T' || separator == 't') && isDate(s, 0, 10) && isTime(s, 11, n);
    }

    /** RFC 4122, as the hexadecimal it is written in. */
    public static boolean uuid(String s) {
        if (s.length() != 36) return false;
        for (int i = 0; i < 36; i++) {
            char c = s.charAt(i);
            if (i == 8 || i == 13 || i == 18 || i == 23) {
                if (c != '-') return false;
            } else if (!isHex(c)) {
                return false;
            }
        }
        return true;
    }

    /** Four octets, written without a leading zero. */
    private static boolean isIpv4(String s, int from, int to) {
        int i = from;
        for (int octet = 0; octet < 4; octet++) {
            if (octet > 0) {
                if (i >= to || s.charAt(i) != '.') return false;
                i++;
            }
            int start = i;
            while (i < to && isDigit(s.charAt(i))) i++;
            int length = i - start;
            if (length < 1 || length > 3) return false;
            if (length > 1 && s.charAt(start) == '0') return false;
            if (number(s, start, i) > 255) return false;
        }
        return i == to;
    }

    /** Four octets, written without a leading zero. */
    public static boolean ipv4(String s) {
        return isIpv4(s, 0, s.length());
    }

    /** Whether every character is ASCII: the half of a host name a name may use. */
    public static boolean ascii(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) > 127) return false;
        }
        return true;
    }

    // Addresses, identifiers and the grammars written over them.

    private static final String SUB_DELIMS = "!$&'()*+,;=";

    /** What a percent escape and the allowed characters answer for one run. */
    private static final int WITH_COLON = 1;
    private static final int WITH_AT = 2;
    private static final int WITH_PATH = 4;
    private static final int WITH_PRIVATE = 8;

    private static boolean isAlpha(char c) {
        return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    }

    /**
     * RFC 3987 ucschar: what an IRI may hold where a URI would demand a percent
     * escape. Every plane below 14 up to its last assignable character, and the
     * one range plane 14 contributes.
     */
    private static boolean isUcschar(int cp) {
        if (cp < 0xA0) return false;
        if (cp <= 0xD7FF) return true;
        if (cp >= 0xF900 && cp <= 0xFDCF) return true;
        if (cp >= 0xFDF0 && cp <= 0xFFEF) return true;
        if (cp >= 0x10000 && cp <= 0xDFFFD) return (cp & 0xFFFF) <= 0xFFFD;
        return cp >= 0xE1000 && cp <= 0xEFFFD;
    }

    /** RFC 3987 iprivate: private use, which only a query may carry. */
    private static boolean isPrivate(int cp) {
        return (cp >= 0xE000 && cp <= 0xF8FF)
            || (cp >= 0xF0000 && cp <= 0xFFFFD)
            || (cp >= 0x100000 && cp <= 0x10FFFD);
    }

    private static boolean isUnreserved(int cp, boolean iri) {
        if (cp < 128) {
            char c = (char) cp;
            return isAlpha(c) || isDigit(c) || c == '-' || c == '.' || c == '_' || c == '~';
        }
        return iri && isUcschar(cp);
    }

    private static boolean isAllowed(int cp, boolean iri, int flags) {
        if (isUnreserved(cp, iri)) return true;
        if (cp < 128 && SUB_DELIMS.indexOf(cp) >= 0) return true;
        if (cp == ':') return (flags & WITH_COLON) != 0;
        if (cp == '@') return (flags & WITH_AT) != 0;
        if (cp == '/' || cp == '?') return (flags & WITH_PATH) != 0;
        return iri && (flags & WITH_PRIVATE) != 0 && isPrivate(cp);
    }

    /**
     * How far a run of allowed characters and percent escapes reaches. A percent
     * that is not an escape ends the run rather than failing, because what may
     * stand after it is the caller's grammar to answer.
     */
    private static int run(String s, int from, int to, boolean iri, int flags) {
        int i = from;
        while (i < to) {
            char c = s.charAt(i);
            if (c == '%') {
                if (i + 2 >= to || !isHex(s.charAt(i + 1)) || !isHex(s.charAt(i + 2))) return i;
                i += 3;
                continue;
            }
            int cp = s.codePointAt(i);
            if (!isAllowed(cp, iri, flags)) return i;
            i += Character.charCount(cp);
        }
        return i;
    }

    /**
     * RFC 4291: eight groups, one of which may be written as `::` and stand for
     * the groups that are zero, and a trailing IPv4 address counts as two.
     */
    private static boolean isIpv6(String s, int from, int to) {
        int i = from;
        int groups = 0;
        boolean compressed = false;
        if (to - i >= 2 && s.charAt(i) == ':' && s.charAt(i + 1) == ':') {
            compressed = true;
            i += 2;
        } else if (i < to && s.charAt(i) == ':') {
            return false;
        }
        while (i < to) {
            int start = i;
            while (i < to && i - start < 4 && isHex(s.charAt(i))) i++;
            if (i == start) return false;
            if (i < to && s.charAt(i) == '.') {
                if (!isIpv4(s, start, to)) return false;
                groups += 2;
                i = to;
                break;
            }
            groups++;
            if (i == to) break;
            if (s.charAt(i) != ':') return false;
            i++;
            if (i < to && s.charAt(i) == ':') {
                if (compressed) return false;
                compressed = true;
                i++;
            } else if (i == to) {
                return false;
            }
        }
        return compressed ? groups <= 7 : groups == 8;
    }

    /** An address written where a name would stand: `[::1]` or `[v7.host]`. */
    private static boolean isIpLiteral(String s, int from, int to, boolean iri) {
        if (to - from >= 2 && (s.charAt(from) == 'v' || s.charAt(from) == 'V')) {
            int i = from + 1;
            int version = i;
            while (i < to && isHex(s.charAt(i))) i++;
            if (i == version || i >= to || s.charAt(i) != '.') return false;
            i++;
            int body = i;
            while (i < to) {
                int cp = s.codePointAt(i);
                if (!(isUnreserved(cp, iri) || (cp < 128 && SUB_DELIMS.indexOf(cp) >= 0) || cp == ':')) {
                    return false;
                }
                i += Character.charCount(cp);
            }
            return i > body;
        }
        return isIpv6(s, from, to);
    }

    /** `[userinfo@]host[:port]`, which ends where the path, query or fragment begins. */
    private static int authority(String s, int from, int to, boolean iri) {
        int i = from;
        int user = run(s, i, to, iri, WITH_COLON);
        if (user < to && s.charAt(user) == '@') i = user + 1;

        if (i < to && s.charAt(i) == '[') {
            int close = s.indexOf(']', i);
            if (close < 0 || !isIpLiteral(s, i + 1, close, iri)) return -1;
            i = close + 1;
        } else {
            i = run(s, i, to, iri, 0);
        }

        if (i < to && s.charAt(i) == ':') {
            i++;
            while (i < to && isDigit(s.charAt(i))) i++;
        }
        return i;
    }

    /** `( "/" segment )*`, where a segment may be empty. */
    private static int segments(String s, int from, int to, boolean iri) {
        int i = from;
        while (i < to && s.charAt(i) == '/') {
            i = run(s, i + 1, to, iri, WITH_COLON | WITH_AT);
        }
        return i;
    }

    /**
     * The part between the scheme and the query: an authority with its path, a
     * path that starts at the root, a path that starts at a segment, or nothing.
     * A reference without a scheme may not open its first segment with a colon,
     * or the colon would read as one.
     */
    private static int hierPart(String s, int from, int to, boolean iri, boolean relative) {
        if (from + 1 < to && s.charAt(from) == '/' && s.charAt(from + 1) == '/') {
            int i = authority(s, from + 2, to, iri);
            if (i < 0) return -1;
            return segments(s, i, to, iri);
        }
        if (from < to && s.charAt(from) == '/') return segments(s, from, to, iri);

        int i = run(s, from, to, iri, WITH_COLON | WITH_AT);
        if (relative) {
            for (int k = from; k < i; k++) if (s.charAt(k) == ':') return -1;
        }
        return segments(s, i, to, iri);
    }

    /** The scheme and its colon, or -1 where the string does not open with one. */
    private static int scheme(String s, int to) {
        if (to == 0 || !isAlpha(s.charAt(0))) return -1;
        int i = 1;
        while (i < to) {
            char c = s.charAt(i);
            if (!(isAlpha(c) || isDigit(c) || c == '+' || c == '-' || c == '.')) break;
            i++;
        }
        return i < to && s.charAt(i) == ':' ? i + 1 : -1;
    }

    private static boolean identifier(String s, boolean iri, boolean relative) {
        int n = s.length();
        int i = 0;
        if (!relative) {
            i = scheme(s, n);
            if (i < 0) return false;
        }
        i = hierPart(s, i, n, iri, relative);
        if (i < 0) return false;
        if (i < n && s.charAt(i) == '?') {
            i = run(s, i + 1, n, iri, WITH_COLON | WITH_AT | WITH_PATH | WITH_PRIVATE);
        }
        if (i < n && s.charAt(i) == '#') {
            i = run(s, i + 1, n, iri, WITH_COLON | WITH_AT | WITH_PATH);
        }
        return i == n;
    }

    /** RFC 4291, on its own rather than inside an authority. */
    public static boolean ipv6(String s) {
        return isIpv6(s, 0, s.length());
    }

    /** RFC 3986: a scheme and everything it names. */
    public static boolean uri(String s) {
        return identifier(s, false, false);
    }

    /** RFC 3986: a URI, or a reference that resolves against one. */
    public static boolean uriReference(String s) {
        return identifier(s, false, false) || identifier(s, false, true);
    }

    /** RFC 3987, which is RFC 3986 with the unreserved characters widened. */
    public static boolean iri(String s) {
        return identifier(s, true, false);
    }

    /** RFC 3987: an IRI, or a reference that resolves against one. */
    public static boolean iriReference(String s) {
        return identifier(s, true, false) || identifier(s, true, true);
    }

    private static boolean isTemplateLiteral(int cp) {
        if (cp < 128) {
            return cp == 0x21 || (cp >= 0x23 && cp <= 0x24) || (cp >= 0x26 && cp <= 0x3B)
                || cp == 0x3D || (cp >= 0x3F && cp <= 0x5B) || cp == 0x5D || cp == 0x5F
                || (cp >= 0x61 && cp <= 0x7A) || cp == 0x7E;
        }
        return isUcschar(cp) || isPrivate(cp);
    }

    private static int varchar(String s, int from, int to) {
        if (from >= to) return -1;
        char c = s.charAt(from);
        if (c == '%') {
            return from + 2 < to && isHex(s.charAt(from + 1)) && isHex(s.charAt(from + 2))
                ? from + 3 : -1;
        }
        return isAlpha(c) || isDigit(c) || c == '_' ? from + 1 : -1;
    }

    /** A variable name, and how much of its value the expression asks for. */
    private static int varspec(String s, int from, int to) {
        int i = varchar(s, from, to);
        if (i < 0) return -1;
        while (true) {
            int dot = i < to && s.charAt(i) == '.' ? i + 1 : i;
            int next = varchar(s, dot, to);
            if (next < 0) break;
            i = next;
        }
        if (i < to && s.charAt(i) == ':') {
            int k = i + 1;
            if (k >= to || s.charAt(k) < '1' || s.charAt(k) > '9') return -1;
            k++;
            for (int digits = 1; digits < 4 && k < to && isDigit(s.charAt(k)); digits++) k++;
            return k;
        }
        if (i < to && s.charAt(i) == '*') return i + 1;
        return i;
    }

    private static int expression(String s, int from, int to) {
        int i = from;
        if (i < to && "+#./;?&=,!@|".indexOf(s.charAt(i)) >= 0) i++;
        while (true) {
            i = varspec(s, i, to);
            if (i < 0) return -1;
            if (i < to && s.charAt(i) == ',') {
                i++;
                continue;
            }
            break;
        }
        return i < to && s.charAt(i) == '}' ? i + 1 : -1;
    }

    /** RFC 6570: literal text and the expressions standing in it. */
    public static boolean uriTemplate(String s) {
        int n = s.length();
        int i = 0;
        while (i < n) {
            char c = s.charAt(i);
            if (c == '{') {
                i = expression(s, i + 1, n);
                if (i < 0) return false;
            } else if (c == '%') {
                if (i + 2 >= n || !isHex(s.charAt(i + 1)) || !isHex(s.charAt(i + 2))) return false;
                i += 3;
            } else {
                int cp = s.codePointAt(i);
                if (!isTemplateLiteral(cp)) return false;
                i += Character.charCount(cp);
            }
        }
        return true;
    }

    /** RFC 6901: slash-separated tokens, in which `~` only escapes `~` and `/`. */
    private static boolean isJsonPointer(String s, int from, int to) {
        int i = from;
        while (i < to) {
            if (s.charAt(i) != '/') return false;
            i++;
            while (i < to && s.charAt(i) != '/') {
                if (s.charAt(i) == '~') {
                    if (i + 1 >= to) return false;
                    char escaped = s.charAt(i + 1);
                    if (escaped != '0' && escaped != '1') return false;
                    i++;
                }
                i++;
            }
        }
        return true;
    }

    /** RFC 6901. */
    public static boolean jsonPointer(String s) {
        return isJsonPointer(s, 0, s.length());
    }

    /** How many levels up, then the pointer from there or the name of the member. */
    public static boolean relativeJsonPointer(String s) {
        int n = s.length();
        if (n == 0) return false;
        char first = s.charAt(0);
        int i = 1;
        if (first >= '1' && first <= '9') {
            while (i < n && isDigit(s.charAt(i))) i++;
        } else if (first != '0') {
            return false;
        }
        if (i == n) return true;
        if (s.charAt(i) == '#') return i + 1 == n;
        return isJsonPointer(s, i, n);
    }

    private static boolean isAtext(int cp, boolean idn) {
        if (cp < 128) return "!#$%&'*+-/=?^_`{|}~".indexOf(cp) >= 0 || isAlpha((char) cp) || isDigit((char) cp);
        return idn && (cp <= 0xD7FF || cp >= 0xE000);
    }

    private static boolean isQuotedLocal(String s, int to, boolean idn) {
        if (to < 2 || s.charAt(to - 1) != '"') return false;
        int i = 1;
        while (i < to - 1) {
            int cp = s.codePointAt(i);
            if (cp == '\\') {
                if (i + 1 >= to - 1) return false;
                char escaped = s.charAt(i + 1);
                if (escaped < 0x20 || escaped > 0x7E) return false;
                i += 2;
                continue;
            }
            boolean plain = (cp >= 0x20 && cp <= 0x21) || (cp >= 0x23 && cp <= 0x5B)
                || (cp >= 0x5D && cp <= 0x7E)
                || (idn && cp >= 0x80 && (cp <= 0xD7FF || cp >= 0xE000));
            if (!plain) return false;
            i += Character.charCount(cp);
        }
        return true;
    }

    /**
     * The half of a mail address before the `@`: dot-separated atoms, or one
     * quoted string. RFC 6531 widens an atom to every character above ASCII when
     * `idn`, and not only to the ones a domain name may hold.
     */
    public static boolean mailLocal(String s, boolean idn) {
        int n = s.length();
        if (n == 0) return false;
        if (s.charAt(0) == '"') return isQuotedLocal(s, n, idn);
        int i = 0;
        while (true) {
            int from = i;
            while (i < n) {
                int cp = s.codePointAt(i);
                if (!isAtext(cp, idn)) break;
                i += Character.charCount(cp);
            }
            if (i == from) return false;
            if (i == n) return true;
            if (s.charAt(i) != '.') return false;
            i++;
        }
    }

    // Mail addresses (RFC 5321 and RFC 6531), and the one question a schema asks

    /**
     * RFC 1123, and an `xn--` label still has to be Punycode that decodes: a
     * host name is the ASCII half of an internationalized one, not a looser
     * grammar.
     */
    public static boolean hostname(String s) {
        return ascii(s) && Idn.hostname(s);
    }

    /** A domain given as an address rather than a name: `[127.0.0.1]` or `[IPv6:::1]`. */
    private static boolean addressLiteral(String domain) {
        int n = domain.length();
        if (n < 2 || domain.charAt(0) != '[' || domain.charAt(n - 1) != ']') return false;
        String body = domain.substring(1, n - 1);
        if (body.regionMatches(true, 0, "ipv6:", 0, 5)) return isIpv6(body, 5, body.length());
        return isIpv4(body, 0, body.length());
    }

    /** A local part the ASCII grammar allows, at a host name or an address. */
    public static boolean email(String s) {
        int at = s.lastIndexOf('@');
        if (at <= 0) return false;
        String domain = s.substring(at + 1);
        return mailLocal(s.substring(0, at), false)
            && (hostname(domain) || addressLiteral(domain));
    }

    /** The same address, with the local part and the host internationalized. */
    public static boolean idnEmail(String s) {
        int at = s.lastIndexOf('@');
        if (at <= 0) return false;
        String domain = s.substring(at + 1);
        return mailLocal(s.substring(0, at), true)
            && (Idn.hostname(domain) || addressLiteral(domain));
    }

    /**
     * Whether `instance` satisfies `format`. An unknown format is satisfied by
     * every string - the specification requires that and callers rely on it -
     * and so is every instance that is not a string at all.
     */
    public static boolean valid(Object format, Object instance) {
        if (!(format instanceof String name) || !(instance instanceof String s)) return true;
        try {
            return switch (name) {
                case "date" -> date(s);
                case "date-time" -> dateTime(s);
                case "time" -> time(s);
                case "duration" -> duration(s);
                case "email" -> email(s);
                case "idn-email" -> idnEmail(s);
                case "hostname" -> hostname(s);
                case "idn-hostname" -> Idn.hostname(s);
                case "ipv4" -> ipv4(s);
                case "ipv6" -> ipv6(s);
                case "uri" -> uri(s);
                case "uri-reference" -> uriReference(s);
                case "iri" -> iri(s);
                case "iri-reference" -> iriReference(s);
                case "uri-template" -> uriTemplate(s);
                case "uuid" -> uuid(s);
                case "json-pointer" -> jsonPointer(s);
                case "relative-json-pointer" -> relativeJsonPointer(s);
                case "regex" -> Regex.ecma(s);
                default -> true;
            };
        } catch (RuntimeException ignored) {
            return false;
        }
    }
}
