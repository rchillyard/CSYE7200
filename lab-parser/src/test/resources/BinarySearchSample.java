package com.phasmidsoftware.dsaipg.misc;

import java.util.Arrays;

/**
 * Class to provide the functionality of binary search.
 * NOTE that it is essentially the same as the Arrays.binarySearch method.
 */
public class BinarySearch {
    /**
     * Main program
     *
     * @param args Command-line args.
     */
    public static void main(String[] args) {
        int[] ar = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // NOTE ar.length, not ar.length - 1: to is exclusive, as it is throughout
        // this repository and as every test here passes it. main had it the other
        // way, so it could not have found the last element.
        int res1 = binarySearch(ar, 0, ar.length, 3);
        System.out.println(res1);
        int res2 = Arrays.binarySearch(ar, 0, ar.length, 3);
        System.out.println(res2);
    }

    /**
     * Method to do binary search.
     *
     * TODO this needs generalizing
     * TODO this returns the wrong result when the key is not found
     * NOTE a corrected version of can be found in the HuskySort repository
     *
     * @param a    the ordered array.
     * @param from the first index on interest.
     * @param to   the first subsequent index that is NOT of interest.
     * @param key  the value we are searching for.
     * @return the index of the element whose value is <code>key</code>, or null if there is no such element.
     */
    static int binarySearch(int[] a, int from, int to, int key) {
        int _from = from;
        int _to = to;
        while (_to > _from) {
            // TO BE IMPLEMENTED  : implement binary search
                        throw new com.phasmidsoftware.dsaipg.util.general.ImplementationMissing();
        }
        return -1;
    }
}
