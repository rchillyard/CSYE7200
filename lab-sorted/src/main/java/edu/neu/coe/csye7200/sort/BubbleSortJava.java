package edu.neu.coe.csye7200.sort;
/*
  (c) Copyright 2018, 2019 Phasmid Software
 */

import java.util.Comparator;

/**
 * Class to sort arrays of (comparable) Xs which extends SortWithHelper and Sort.
 *
 * @param <X> the underlying type of elements to be sorted (must support Comparable).
 */
public class BubbleSortJava<X> {

    public BubbleSortJava(Comparator<X> comparator) {
        this.comparator = comparator;
    }

    /**
     * Sort the sub-array xs:from:to using bubble sort.
     *
     * @param xs   sort the array xs from "from" to "to".
     */
    public void sort(X[] xs) {
        sort(xs, 0, xs.length);
    }

    /**
     * Sort the sub-array xs:from:to using bubble sort.
     *
     * @param xs   sort the array xs from "from" to "to".
     * @param from the index of the first element to sort.
     * @param to   the index of the first element not to sort.
     */
    public void sort(X[] xs, int from, int to) {
        for (int j = to; j > from; j--)
            if (optimizedInnerLoopSuccess(xs, from, j))
                break;
    }

    /**
     * "Optimized" inner loop of bubble sort (see Wikipedia: <a href="https://en.wikipedia.org/wiki/Bubble_sort#Implementation">Bubble sort implementation</a>)
     * The optimization is that we only loop until we reach the (j-1)th element because the jth element and beyond
     * cannot possibly be out of order.
     *
     * @param xs   the complete array to be sorted.
     * @param from the index of the first element to sort.
     * @param j    the index of the first element not to sort.
     * @return true if we passed through the elements without swapping any.
     */
    private boolean optimizedInnerLoopSuccess(X[] xs, int from, int j) {
        boolean swapped = false;
        for (int i = from + 1; i < j; i++) swapped |= swapStableConditional(xs, i);
        return !swapped;
    }

    private boolean swapStableConditional(X[] xs, int i) {
        final X v = xs[i - 1];
        final X w = xs[i];
        boolean result = comparator.compare(w,v) < 0;
        if (result) {
            xs[i] = v;
            xs[i - 1] = w;
        }
        return result;
    }

    private final Comparator<X> comparator;

//    public static final String DESCRIPTION = "Bubble sort";

}