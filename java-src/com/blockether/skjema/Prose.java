package com.blockether.skjema;

/**
 * The prose of one error, in one allocation.
 *
 * <p>An explained instance pays for the words of every error it answers, and the
 * words are the largest thing a report allocates. Built from Clojure the cost is
 * paid three times over: {@code clojure.core/str} renders each argument into its
 * own String, a StringBuilder guesses a capacity, and {@code toString} copies the
 * result out of it. Built here, javac renders {@code +} as an invokedynamic
 * concatenation that measures the arguments first and allocates the answer once -
 * measured at 5 ns against 21 ns for an error location, and 15 ns against 28 ns
 * for a sentence of four parts.
 *
 * <p>Nil reads as the empty string, exactly as {@code clojure.core/str} words it,
 * so the two ways of saying the same refusal cannot drift apart.
 */
public final class Prose {
    private Prose() {}

    private static String text(Object value) {
        return value == null ? "" : value.toString();
    }

    /** One sentence of three parts. */
    public static String words(Object a, Object b, Object c) {
        return text(a) + text(b) + text(c);
    }

    /** One sentence of four parts. */
    public static String words(Object a, Object b, Object c, Object d) {
        return text(a) + text(b) + text(c) + text(d);
    }

    /** One sentence of five parts. */
    public static String words(Object a, Object b, Object c, Object d, Object e) {
        return text(a) + text(b) + text(c) + text(d) + text(e);
    }

    /** The location of a member, from the location of the object it stands in. */
    public static String member(String location, String token) {
        return location + "/" + token;
    }

    /** The location of an item, from the location of the array it stands in. */
    public static String index(String location, long index) {
        return location + "/" + index;
    }
}
