package edu.neu.coe.csye7200.fp

import java.io.{BufferedWriter, FileWriter, Flushable}

/**
 * This object provides code which behaves as a functional debugger.
 * That's to say that the result of the performing the debug is not Unit
 * (as may be the case with a statement-oriented language like Java)
 * but is the same as the input.
 */
object DebugF {

    /**
     * Here, we need an implicit class based on a String.
     * That's to say, we want to invoke this behavior whenever we write:
     *
     *     some-string !! some-object
     *
     * Because the !! method is not available for String, but IS available for Debugger.
     *
     * But there's a wrinkle. We want to be able to pick up different implicit
     * Writable objects.
     *
     * @param w the string which will be the prefix of the written-out debug/log message.
     * @param z the (implicit) Writable that the debug/log message will be written to.
     */
    implicit class Debugger(w: String)(implicit z: Writable) {

        /**
         * Method to invoke a debug/log message made up of:
         *     a prefix ("DebugF") and colon (:) followed by
         *     the message (w) and colon (:) followed by the value of x.
         *
         * @param x the object to be logged and returned.
         * @tparam X the type of x.
         * @return x.
         */
        def !![X](x: X): X = { /* SOLUTION */
            ???
            /* END */
        }

        /**
         * Method to not invoke a debug/log message.
         * The reason we need this method is that occasionally,
         * we want to turn off actually debug/logging but without dismantling the log message and overall structure of the code.
         *
         * @param x the object to be returned.
         * @tparam X the type of x.
         * @return x.
         */
        def !|[X](x: X): X =  /* SOLUTION */ ??? /* END */
    }
}

/**
 * This trait knows how to write Strings to some sink of type Sink.
 * For some sink types, it can even read back the String.
 * This is especially useful for unit testing Writables.
 */
trait Writable {

    /**
     * Type alias for something which is Appendable, AutoCloseable and Flushable.
     */
    type Sink = Appendable with AutoCloseable with Flushable

    /**
     * Method which first appends w to sink; then it appends newline ("\n") to sink; finally it flushes sink.
     *
     * @param w a String to be written out.
     */
    def write(w: String): Unit = {
        /* SOLUTION */
        /* END */
    }

    /**
     * The sink for this Writable.
     *
     * @return a Sink.
     */
    val sink: Sink

    /**
     * It possible, get back the String(s) already written to the sink.
     *
     * @return a String or throw an exception.
     */
    def readBack: String

    /**
     * This method will close this Writable so that it can no longer return any content.
     */
    def close(): Unit
}

object Writable {

    /**
     * Implicit Writable where the sink is System.out.
     * ReadBack is not possible with this sink.
     */
    implicit object SysOutWritable extends Writable {
        /* SOLUTION */
        val sink: Sink = ???

        def readBack: String = ???
        /* END */

        def close(): Unit = ()
    }


    /**
     * Implicit Writable where the sink is a file called logFile.
     * ReadBack is not possible with this sink.
     */
    implicit object LogFileWritable extends Writable {
        var lastMessage: String = _ // NOTE: Yes, it's OK to use a var here.

        val sink: Sink = new BufferedWriter(new FileWriter("logFile", true))

        /* SOLUTION */

        override def write(w: String): Unit = {
            /* SOLUTION */ /* END */
        }

        def readBack: String = /* SOLUTION */ ???
        /* END */

        def close(): Unit = /* SOLUTION */ /* END */
    }

    implicit object StringBuilderWritable extends Writable {
        private val sb = new StringBuilder()

        val sink: Sink = new StringBuilderSink(sb)

        def readBack: String = sb.toString()

        def close(): Unit = sb.clear()
    }

    /**
     * Class to define a sink for use with StringBuilderSink.
     *
     * @param sb a StringBuilder.
     */
    class StringBuilderSink(sb: StringBuilder) extends Appendable with AutoCloseable with Flushable {
        def append(csq: CharSequence): Appendable = {
            sb.append(csq)
            this
        }

        def append(csq: CharSequence, start: Int, end: Int): Appendable = {
            sb.append(csq, start, end)
            this
        }

        def append(c: Char): Appendable = {
            sb.append(c)
            this
        }

        def close(): Unit = ()

        def flush(): Unit = ()
    }
}

