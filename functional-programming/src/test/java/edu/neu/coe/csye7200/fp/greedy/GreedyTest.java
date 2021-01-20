///*
// * Copyright (c) 2018. Phasmid Software
// */
//
//package edu.neu.coe.csye7200.fp.greedy;
//
//import org.junit.Test;
//
//import java.util.ArrayList;
//import java.util.Iterator;
//
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertFalse;
//
//public class GreedyTest {
//
//    /**
//     * Class to model Zeckendorf's theorem.
//     * See also Zeckendorf.java in the main tree.
//     * This version of Zeckendorf uses the Greedy class.
//     */
//    class GreedyZ {
//
//        GreedyZ() {
//            greedy = new Greedy<>(
//                    this::getLargestFibonacci,
//                    (Long l1, Long l2) -> l1 - l2,
//                    this::concatenate,
//                    (Long l) -> l <= 0
//            );
//        }
//
//        /**
//         * Method to run the greedy algorithm defined by greedy.
//         *
//         * @param x the value for which we want the Zeckendorf representation.
//         * @return the Zeckendorf representation.
//         */
//        ArrayList<Long> run(Long x) {
//            return greedy.apply(x, new ArrayList<>());
//        }
//
//        // We have to do it this cumbersome way because Java is not really a functional language
//        private ArrayList<Long> concatenate(Long l, ArrayList<Long> ls) {
//            ArrayList<Long> r = new ArrayList<>(ls);
//            r.add(l);
//            return r;
//        }
//
//        // Get the largest Fibonacci number which is no bigger than x
//        private long getLargestFibonacci(long x) {
//            return Fibonacci.getLargest(x);
//        }
//
//        private final Greedy<Long, ArrayList<Long>> greedy;
//    }
//
//    /**
//     * Test method for get
//     */
//    @Test
//    public void testGet0() {
//        GreedyZ target = new GreedyZ();
//        Iterator<Long> fibs = target.run(5L).iterator();
//        assertEquals(new Long(5), fibs.next());
//        assertFalse(fibs.hasNext());
//    }
//
//    /**
//     * Test method for get
//     */
//    @Test
//    public void testGet1() {
//        GreedyZ target = new GreedyZ();
//        Iterator<Long> fibs = target.run(10L).iterator();
//        assertEquals(new Long(8), fibs.next());
//        assertEquals(new Long(2), fibs.next());
//        assertFalse(fibs.hasNext());
//    }
//
//    /**
//     * Test method for get
//     */
//    @Test
//    public void testGet2() {
//        GreedyZ target = new GreedyZ();
//        Iterator<Long> fibs = target.run(100L).iterator();
//        assertEquals(new Long(89), fibs.next());
//        assertEquals(new Long(8), fibs.next());
//        assertEquals(new Long(3), fibs.next());
//        assertFalse(fibs.hasNext());
//    }
//
//    /**
//     * Test method for get
//     */
//    @Test
//    public void testGet3() {
//        GreedyZ target = new GreedyZ();
//        Iterator<Long> fibs = target.run(1000L).iterator();
//        assertEquals(new Long(987), fibs.next());
//        assertEquals(new Long(13), fibs.next());
//        assertFalse(fibs.hasNext());
//    }
//
//    /**
//     * Test method for get
//     */
//    @Test
//    public void testGet4() {
//        GreedyZ target = new GreedyZ();
//        Iterator<Long> fibs = target.run(10000L).iterator();
//        assertEquals(new Long(6765), fibs.next());
//        assertEquals(new Long(2584), fibs.next());
//        assertEquals(new Long(610), fibs.next());
//        assertEquals(new Long(34), fibs.next());
//        assertEquals(new Long(5), fibs.next());
//        assertEquals(new Long(2), fibs.next());
//        assertFalse(fibs.hasNext());
//    }
//
//}
