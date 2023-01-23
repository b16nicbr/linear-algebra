/**
* University of Skövde
* Course: Advanced Programming
*/

package linalg

/**
* This Matrix class represent (basically) a collection of vectors.
*
* @param data Where the values of the vector are held.
*/
class Mat(val data: Lista[Vec]) {

  // ============== Some already implemented methods  ===============

  /**
  * Returns the number of elements in the matrix.
  *
  * @return number of elements.
  */
  def size: Int = data.map(_.size).reduce(_ + _)

  /**
  * Returns the shape in terms of rows and columns.
  *
  * @return a tuple, where the first element are the number of rows, and the
  *   second the number of columns.
  */
  def shape: (Int, Int) = (data.size, data.head.size)

  /**
  * Compares whether this matrix is equal to another in terms of its elements.
  * Mainly required for testing.
  *
  * @param other the other matrix with which to compare.
  *
  * @return true if shape and elements are equal, false otherwise.
  */
  def equals(other: Mat): Boolean = {
    shape == other.shape &&
    data.zip(other.data).forall{ case (a: Vec, b: Vec) => a equals b}
  }

  /**
  * Prettyfies the output of a matrix.
  */
  override def toString: String = {
    data.map(_.toString).reduce(_ + "\n" + _)
  }


  // =================================================================
  // ===================== Your working area =========================

  // ------- Selecting and slicing -------

  /**
  * Returns the ith row (if axis == 0) or a column (if axis == 1).
  *
  * @param i the ith row or column that is to be returned.
  * @param axis 0 for selecting a row, 1 for selecting a column.
  *
  * @return a vector corresponding to the selected row or column.
  */
  def apply(i: Int, axis: Int): Vec = {
    def apply( index: Int, axis: Int, vec:Lista[Vec]):Lista[Double]= (axis, vec) match{
      case (_, Emp)=> Emp
      case (1,Cons(head,tail)) => Cons(head.data.apply(i), apply(index +1, axis, tail))
      case (_, Cons(_,_)) =>Emp
    }

    if (axis == 0) data.apply( i ) else
      Vec( apply( i, axis, data ) )
  }

  /**
  * Returns the rows (if axis == 0), or columns (if axis == 1), between the given
  * range (inclusive). Check the scala API for Range.
  */
  def apply(range: Range, axis: Int): Mat ={

    def apply(startEnd: Range, iter: Int, axis: Int, nl: Mat): Lista[Vec] = {
      if (startEnd.start > iter)
        apply( startEnd, iter + 1, axis, nl )
      else if (startEnd.`end` + 1 > iter && startEnd.start <= iter)
        Cons( nl.apply( iter, axis ), apply( startEnd, iter + 1, axis, nl ) )
      else Emp
    }
      val mat = Mat( apply( range, 0, axis, this ) )
      if (axis == 1) {
        mat.t
      } else mat
  }


  // ------- Element-wise operations -------

  /**
  * The following operations are element-wise. That is, each opeartion will
  * result in a new matrix of the same shape, but with each element equal to
  * the corresponding operation.
  * There are three groups: operations with Doubles, with Vectors, and with Matrices.
  * Examples:
  *   m = [[1, 1], [2, 2]]
  *   v = [2, 2]
  *
  *   With doubles:
  *   m + 1 == [[2, 2], [3, 3]]
  *   m - 1 == [[0, 0], [1, 1]]
  *
  *   With Vectors:
  *   m * v == [[2, 2], [4, 4]]
  *
  *   With Matrices:
  *   m / m == [[1, 1], [1, 1]]
  *
  * Notes:
  * - For Vector operations you may assume that the size of the vector is
  *   equal to the number of columns in the matrix.
  * - For Matrix operations you may assume that both matrices are of the same shape.
  * - Take into account that subtraction and division are non-associative! (v - m != m - v).
  */

  def +(x: Double): Mat = {
    def plus(x: Double, vec:Lista[Vec]): Lista[Vec] =  vec match {
      case Emp =>throw new Exception("Empty list")
      case Cons(head, Emp) => Cons(head + x, Emp)
      case Cons(head, tail) => Cons(head +x, plus(x,tail))
    }
    Mat(plus(x, Cons(data.head, data.tail)))
  }
  def *(x: Double): Mat = {
    def multiple(x: Double, vec: Lista[Vec]): Lista[Vec] = vec match {
      case Emp => throw new Exception( "Empty list" )
      case Cons( head, Emp ) => Cons( head * x, Emp )
      case Cons( head, tail ) => Cons( head * x, multiple( x, tail ) )
    }

    Mat( multiple( x, Cons( data.head, data.tail ) ) )
  }
  def -(x: Double): Mat = {
    def minus(x: Double, vec: Lista[Vec]): Lista[Vec] = vec match {
      case Emp => throw new Exception( "Empty list" )
      case Cons( head, Emp ) => Cons( head - x, Emp )
      case Cons( head, tail ) => Cons( head - x, minus( x, tail ) )
    }

    Mat( minus( x, Cons( data.head, data.tail ) ) )
  }
  def /(x: Double): Mat = {
    def division(x: Double, vec: Lista[Vec]): Lista[Vec] = vec match {
      case Emp => throw new Exception( "Empty list" )
      case Cons( head, Emp ) => Cons( head / x, Emp )
      case Cons( head, tail ) => Cons( head / x, division( x, tail ) )
    }
    Mat( division( x, Cons( data.head, data.tail ) ) )
  }

  def +(x: Vec): Mat = Mat(data.map(_.+(x)))
  def *(x: Vec): Mat = Mat(data.map(_.*(x)))
  def -(x: Vec): Mat = Mat(data.map(_.-(x)))
  def /(x: Vec): Mat = Mat(data.map(_./(x)))

  def +(x: Mat): Mat = {
    def plus(x:Lista[Vec], vec:Lista[Vec]): Lista[Vec] = vec match{
      case Emp => throw new Exception("Empty list")
      case Cons(head, Emp) => Cons(head +x.head, Emp)
      case Cons(head, tail) => Cons(head + x.head, plus(x,tail))
    }
    Mat(plus(x.data,Cons(data.head,data.tail)))
  }

  def *(x: Mat): Mat = {
    def multiple(x: Lista[Vec], vec: Lista[Vec]): Lista[Vec] = vec match {
      case Emp => throw new Exception( "Empty list" )
      case Cons( head, Emp ) => Cons( head * x.head, Emp )
      case Cons( head, tail ) => Cons( head * x.head, multiple( x, tail ) )
    }

    Mat( multiple( x.data, Cons( data.head, data.tail ) ) )
  }
  def -(x: Mat): Mat = {
    def minus(x: Lista[Vec], vec: Lista[Vec]): Lista[Vec] = vec match {
      case Emp => throw new Exception( "Empty list" )
      case Cons( head, Emp ) => Cons( head - x.head, Emp )
      case Cons( head, tail ) => Cons( head - x.head, minus( x, tail ) )
    }

    Mat( minus( x.data, Cons( data.head, data.tail ) ) )
  }
  def /(x: Mat): Mat = {
    def division(x: Lista[Vec], vec: Lista[Vec]): Lista[Vec] = vec match {
      case Emp => throw new Exception( "Empty list" )
      case Cons( head, Emp ) => Cons( head / x.head, Emp )
      case Cons( head, tail ) => Cons( head / x.head, division( x, tail ) )
    }

    Mat( division( x.data, Cons( data.head, data.tail ) ) )
  }


  /**
  * Sums each element in the matrix.
  */
  def sum: Double = data.map(_.sum).reduce(_+_)


  // ------- Transposing -------

  /**
  * Transposes the matrix (see exercise 6.6).
  */
  def t: Mat = {
    def helperT(startEnd: Range, iter: Int, axis: Int, nl: Mat): Lista[Vec] = {
      if (startEnd.start > iter)
        helperT( startEnd, iter + 1, axis, nl )
      else if (startEnd.`end` + 1 > iter && startEnd.start <= iter)
        Cons( nl.apply( iter, axis ), helperT( startEnd, iter + 1, axis, nl ) )
      else Emp
    }

    Mat( helperT( 0 to data.head.data.size - 1, 0, 1, this ) )


  }



  // ------- Dot product / Matrix multiplication -------

  /**
  * Performs a row-wise dot product (see exercise 2.11).
  */
  def dot(vec: Vec): Mat = {
    def dotRow (m_row: Lista[Vec]): Lista[Vec] = m_row match {
      case Emp => Emp
      case Cons( h, t ) => Cons( Vec( h.dot( vec ) ), dotRow( t ) )
    }
    Mat( dotRow( data ) )
  }

  /**
  * Performs a matrix mutiplication (see exercise 6.7)
  */
  def dot(m: Mat): Mat = {
    def dotMult(m:Mat, vec:Lista[Vec]):Lista[Vec] = vec match {
      case Emp => throw new Exception("Empty list")
      case Cons(head, tail) => Cons(m.dot(head).t.data.head, dotMult(m, tail))
    }
    Mat(dotMult(m.t, Cons(data.head, data.tail)))
  }

}



/**
* Companion object of Mat.
* Defines comodity methods for instantiating a matrix.
*/
object Mat {
  def apply(vecs: Lista[Vec]): Mat = new Mat(vecs)

  def apply(vecs: Vec*): Mat = Mat(Lista(vecs:_*))
}
