/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.util;

import java.util.function.Function;

/**
 * @param <T> The generic type T is that of the input to the function f which you will pass in to the constructor.
 */
public class Benchmark<T> {

    /**
     * Constructor for a Benchmark with option of specifying all three functions.
     *
     * @param fPre  a function of T => T.
     *              Function fPre is run before each invocation of fRun (but with the clock stopped).
     *              The result of fPre (if any) is passed to fRun.
     * @param fRun  a function of T => Void.
     *              Function fRun is the function whose timing you want to measure. For example, you might create a function which sorts an array.
     *              When you create a lambda defining fRun, you must return "null."
     * @param fPost a function of T => Void.
     *              Function fPost is run after each invocation of fRun (but with the clock stopped).
     */
    public Benchmark(Function<T, T> fPre, Function<T, Void> fRun, Function<T, Void> fPost) {
        this.fPre = fPre;
        this.fRun = fRun;
        this.fPost = fPost;
    }

    /**
     * Constructor for a Benchmark with option of specifying all three functions.
     *
     * @param fPre a function of T => T.
     *             Function fPre is run before each invocation of fRun (but with the clock stopped).
     *             The result of fPre (if any) is passed to fRun.
     * @param fRun a function of T => Void.
     *             Function fRun is the function whose timing you want to measure. For example, you might create a function which sorts an array.
     *             When you create a lambda defining fRun, you must return "null."
     */
    public Benchmark(Function<T, T> fPre, Function<T, Void> fRun) {
        this(fPre, fRun, null);
    }

    /**
     * Constructor for a Benchmark where only the (timed) run function is specified.
     *
     * @param f a function of T => Void.
     *          Function f is the function whose timing you want to measure. For example, you might create a function which sorts an array.
     *          When you create a lambda defining f, you must return "null."
     */
    public Benchmark(Function<T, Void> f) {
        this(null, f);
    }

    /**
     * Run function f m times and return the average time in milliseconds.
     *
     * @param t the value that will in turn be passed to function f.
     * @param m the number of times the function f will be called.
     * @return the average number of milliseconds taken for each run of function f.
     */
    public double run(T t, int m) {
        // Warmup phase
        int warmupRuns = Integer.min(2, Integer.max(10, m / 10));
        for (int i = 0; i < warmupRuns; i++) doRun(t, true);
        // Timed phase
        long totalTime = 0;
        for (int i = 0; i < m; i++) totalTime += doRun(t, false);
        return (double) totalTime / m / 1000000;
    }

    private long doRun(T t, boolean warmup) {
        T t1 = fPre != null ? fPre.apply(t) : t;
        if (warmup) {
            fRun.apply(t1);
            return 0;
        }
        long start = System.nanoTime();
        fRun.apply(t1);
        long nanos = System.nanoTime() - start;
        if (fPost != null) fPost.apply(t1);
        return nanos;
    }

    private final Function<T, T> fPre;
    private final Function<T, Void> fRun;
    private final Function<T, Void> fPost;

}
