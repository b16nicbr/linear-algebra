/**
* University of Skövde
* Course: Advanced Programming
*/

package linalg

import scala.math.Fractional.Implicits.infixFractionalOps


/**
* This Vector class represent a specialized collection of (for simplicity) just
* Doubles.
*
* @param data Where the values of the vector are held.
*/
class Vec(val data: Lista[Double]) {

  // ============== Some already implemented methods  ===============

  /**
  * Returns the number of elements in the vector.
  *
  * @return number of elements.
  */
  def size: Int = data.size

  /**
  * Returns the shape in terms of rows and columns.
  *
  * @return a tuple, where the first element are the number of rows (1 in this case),
  * and the second the number of columns (= size).
  */
  def shape: (Int, Int) = (1, data.size)

  /**
  * Compares whether this vector is equal to another in terms of its elements.
  * Mainly required for testing.
  *
  * @return true if shape and elements are equal, false otherwise.
  */
  def equals(other: Vec): Boolean = data == other.data

  /**
  * Prettyfies the output of a matrix.
  */
  override def toString: String = data.toString


  // =================================================================
  // ===================== Your working area =========================

  // ------- Selecting and slicing -------

  /**
  * Returns the ith element.
  *
  * @param i index in the vector (0-indexed).
  * @return the ith element in the vector.
  */
  def apply(i: Int): Double = {
    def apply(ith: Int, vec: Vec): Double = {
      if (ith == 0){
        vec.data.head
      }else{
        apply(ith-1,Vec(vec.data.tail))
      }
    }
    apply(i,Vec(data))

  }

  /**
  * Returns the ith element.
  *
  * @param i index in the vector (0-indexed).
  * @return the ith element in the vector.
  */
  def apply(range: Range): Vec = {
    def apply(range:Range, vec: Vec, rangeInt: Int, pos1: Int, pos2: Int): Vec = {
      if((range.start == pos1 && range.end == pos1) ) {
        Vec(vec.data.apply(pos1))
      }
      else if (range.start==0 && range.end<=pos2) {
        Vec( vec.data.take( pos2 ) )
      }
      else if (range.start >= 1 && range.end >= pos2) {
        Vec( vec.data.drop( pos1 ) )
      }else {
        apply(range,Vec(vec.data.tail),rangeInt-1, pos1, pos2)
      }
    }
    apply(range,Vec(data), data.size, range.start, range.end)
  }



  // ------- Element-wise operations -------

  /**
  * The following operations are element-wise. That is, each opeartion will
  * result in a new vector (or matrix) of the same shape, but with each element equal to
  * the corresponding operation.
  * There are three groups: operations with Doubles, with Vectors, and with Matrices.
  * Examples:
  *   m = [[1, 1], [2, 2]]
  *   v = [2, 3]
  *
  *   With doubles:
  *   v + 5 == [7, 8]
  *   m - 5 == [-3, -2]
  *
  *   With Vectors:
  *   v * v == [4, 6]
  *
  *   With Matrices:
  *   v / m == [[2, 3], [1, 1.5]]
  *
  * Notes:
  * - For Vector operations you may assume that the sizes are equal.
  * - For Matrix operations you may assume that the size of this vector is equal
  *   to the number of columns in the matrix.
  * - Take into account that subtraction and division are non-associative! (v - m != m - v).
  */

  def +(x: Double): Vec = {
    def plus(x:Double, vec:Lista[Double], resultVec:Lista[Double]): Vec = vec match {
      case Emp => Vec(resultVec.reverse)
      case Cons(head,Emp) => plus(x,Emp,Cons(head+x, resultVec))
      case Cons(head, tail) =>  plus(x, tail, Cons(head+ x, resultVec))
    }
    plus(x,data,Lista())
  }
  def +(x: Vec): Vec = {
    def plus(x: Lista[Double], vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons(head, Emp) => plus( x, Emp, Cons( head + x.head, resultVec ) )
      case Cons(head, tail)=> plus( x.tail, tail, Cons( head +x.head, resultVec ) )
    }

    plus( x.data, data, Lista() )
  }
  def +(x: Mat): Mat = Mat(x.data.map(_.+(Vec(data))))

  def *(x: Double): Vec = {
    def multiple(x: Double, vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons( head, Emp ) => multiple( x, Emp, Cons( head * x, resultVec ) )
      case Cons( head, tail ) => multiple( x, tail, Cons( head * x, resultVec ) )
    }
    multiple(x,data, Lista())
  }
  def *(x: Vec): Vec = {
    def multiple(x: Lista[Double], vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons( head, Emp ) => multiple( x, Emp, Cons( head * x.head, resultVec ) )
      case Cons( head, tail ) => multiple( x.tail, tail, Cons( head * x.head, resultVec ) )
    }

    multiple( x.data, data, Lista() )
  }
  def *(x: Mat): Mat = Mat(x.data.map(_.*(Vec(data))))

  def -(x: Double): Vec = {
    def minus(x: Double, vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons( head, Emp ) => minus( x, Emp, Cons( head - x , resultVec ) )
      case Cons( head, tail ) => minus( x, tail, Cons( head -x , resultVec ) )
    }
    minus( x, data, Lista() )
  }

  def -(x: Vec): Vec = {
    def minus(x: Lista[Double], vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons( head, Emp ) => minus( x, Emp, Cons( head - x.head, resultVec ) )
      case Cons( head, tail ) => minus( x, tail, Cons( head - x.head, resultVec ) )
    }

    minus( x.data, data, Lista() )
  }
  def -(x: Mat): Mat = Mat(x.data.map(_.-(Vec(data))))

  def /(x: Double): Vec = {
    def division(x: Double, vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons( head, Emp ) => division( x, Emp, Cons( head / x, resultVec ) )
      case Cons( head, tail ) => division( x, tail, Cons( head / x, resultVec ) )
    }
    division( x, data, Lista() )
  }
  def /(x: Vec): Vec = {
    def division(x: Lista[Double], vec: Lista[Double], resultVec: Lista[Double]): Vec = vec match {
      case Emp => Vec( resultVec.reverse )
      case Cons( head, Emp ) => division( x, Emp, Cons( head / x.head, resultVec ) )
      case Cons( head, tail ) => division( x, tail, Cons( head / x.head, resultVec ) )
    }
    division(x.data, data,Lista())
  }
  def /(x: Mat): Mat = Mat(x.data.map(_./(Vec(data))))


  /**
  * Sums all elements in the vector.
  */
  def sum: Double = {
    def sum(vec: Vec, add: Double) :Double = vec match {
      case _ if vec.size == 0 => add
      case vec => sum(Vec(vec.data.tail),vec.data.head + add)
    }
     sum(Vec(data),0)
  }


  // ------- Dot product / Matrix multiplication -------

  /**
  * Performs dot product (see exercise 2.11).
  */
  def dot(vec: Vec): Double = {
    def dotProd(ls1: Lista[Double], ls2: Lista[Double]): Double = (ls1, ls2) match {
      case (Emp, Emp) => throw new Exception( "empty lists" )
      case (Cons( h1, Emp ), Cons( h2, Emp )) => h1 * h2
      case (Cons( h1, t1 ), Cons( h2, t2 )) => h1 * h2 + dotProd( t1, t2 )
    }
    dotProd( data, vec.data )
  }

  /**
  * Performs a row-wise dot product (see exercise 2.11).
  */
  def dot(m: Mat): Vec = {
    def dotRow(m: Lista[Vec]): Lista[Double] = m match {
      case Emp => Emp
      case Cons( h, t ) => Cons(dot(h) , dotRow( t ) )
    }
    Vec( dotRow( m.data ) )
  }

}



/**
* Companion object of Vec.
* Defines comodity methods for instantiating a vector.
*/
object Vec {
  def apply(ns: Lista[Double]): Vec = new Vec(ns)

  def apply(ns: Double*): Vec = Vec(Lista(ns:_*))
}
