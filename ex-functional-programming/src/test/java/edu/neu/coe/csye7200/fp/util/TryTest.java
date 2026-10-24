/*
 * Copyright (c) 2026. Phasmid Software
 */
package edu.neu.coe.csye7200.fp.util;

import scala.runtime.BoxedUnit;
import scala.util.Try;

public class TryTest {

    @org.junit.Test
    public void testTry() {
        Try<BoxedUnit> good = Trial.trial(true);
        Try<BoxedUnit> bad  = Trial.trial(false);

        String goodMsg = good.fold(
                ex -> "failed: " + ex.getLocalizedMessage(),
                u  -> "good is OK"
        );
        String badMsg = bad.fold(
                ex -> "failure message: " + ex.getLocalizedMessage(),
                u  -> "bad is OK"
        );

        System.out.println(goodMsg);
        System.out.println(badMsg);
    }
}
