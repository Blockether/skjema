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
    public static boolean ipv4(String s) {
        int n = s.length();
        int i = 0;
        for (int octet = 0; octet < 4; octet++) {
            if (octet > 0) {
                if (i >= n || s.charAt(i) != '.') return false;
                i++;
            }
            int from = i;
            while (i < n && isDigit(s.charAt(i))) i++;
            int length = i - from;
            if (length < 1 || length > 3) return false;
            if (length > 1 && s.charAt(from) == '0') return false;
            if (number(s, from, i) > 255) return false;
        }
        return i == n;
    }

    /** Whether every character is ASCII: the half of a host name a name may use. */
    public static boolean ascii(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) > 127) return false;
        }
        return true;
    }
}
