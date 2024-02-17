/**
 * Trait to define a two-dimensional matrix where the elements are of type X.
 * A matrix (if you've forgotten) is a rectangular structure of elements (the cells) arranged in a grid of intersecting rows and columns.
 * By convention, the top row is numbered 0, and the leftmost column is numbered 0.
 *
 * @tparam X the underlying type of the elements.
 */
trait Matrix2[+X] extends PartialFunction[(Int, Int), X] {

    self =>

    def isDefinedAt(x: (Int, Int)) = x._1 >= 0 && x._1 < rows && x._2 >= 0 && x._2 < cols

    def apply(x: (Int, Int)): X

    def rows: Int

    def cols: Int

    def transpose: Matrix2[X] = new Matrix2[X] {
        def apply(x: (Int, Int)) = self(x.swap)

        def rows = self.cols

        def cols = self.rows
    }
}

/**
 * The elements of this matrix are encrypted. You cannot access them other than through the apply method.
 *
 * However, for testing purposes, you may of course implement the apply function in a way that makes sense.
 *
 * @param bytes this is the encrypted data that will be used to get values for the matrix cells.
 */
class EncryptedIntMatrix(bytes: Array[Byte]) extends Matrix2[Int] {
    def apply(x: (Int, Int)) = ???

    def rows = ???

    def cols = ???

    /**
     * Multiply (dot product) row "row" from this Matrix2 with column "col" from other Matrix2.
     * The dot product is the sum of the products ai * bi over all valid values of i and where a is a row from the
     * matrix on the left (this) and b is a column from the matrix on the right (other).
     * Don't forget to check that the row and column have the same length.
     *
     * As always, points awarded for elegance. In particular, you should avoid using var.
     *
     * @param other the other Matrix2.
     * @param row   the row from this Matrix2.
     * @param col   the column from other Matrix2.
     * @return the dot product of the row and the column.
     */
    def dotProduct(other: Matrix2[Int])(row: Int, col: Int): Long = {
        val myRowFunction: Int => Long = apply(row, _)
        val otherColumnFunction: Int => Long = other.apply(_, col)
        val z1: IndexedSeq[Long] = for (i <- 0 to cols) yield myRowFunction(i)
        val z2: IndexedSeq[Long] = for (i <- 0 to other.rows) yield otherColumnFunction(i)
        (z1 zip z2).map(x => x._1 * x._2).sum
    }
    // TODO implement dotProduct (26)
}

val cipherText = "jdkjaiopequwiojsaidnfashpidpqlle"
val matrixA = new EncryptedIntMatrix(cipherText.getBytes)

/**
 * Method "select" to yield a function which takes a Matrix2, and the row and column values and yields an X:
 *
 * @tparam X the element type.
 * @return the value of the element at row, column given by the 2nd and 3rd bound variables.
 */
def select[X]: (Matrix2[X], Int, Int) => X = (m, r, c) => m(r -> c) // TODO 6 points

/**
 * Method "transpose" which is the same as select, only operates on the transpose of the matrix,
 * i.e. where rows and columns have been exchanged.
 * You MUST implement transpose in terms of select (don't use the transpose method of Matrix2).
 *
 * @tparam X the element type.
 * @return the value of the element at row, column given by the 3rd and 2nd bound variables.
 */
def transpose[X]: (Matrix2[X], Int, Int) => X = (m, r, c) => select(m, c, r) // TODO 8 points