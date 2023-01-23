/**
* University of Skövde
* Course: Advanced Programming
*/

package object linalg {

  /**
  * This implict class will allow us to perform operations where Double comes first
  * e.g., 3 - Vec(1, 2, 3).
  */
  implicit class IntWrapper(n: Double) {

    /**
    * Since addition and multiplication are associative, we can just call
    * their corresponding implementation in Vec and Mat.
    */
    def +(x: Vec): Vec = x + n
    def +(x: Mat): Mat = x + n
    def *(x: Vec): Vec = x * n
    def *(x: Mat): Mat = x * n


    /**
    * These ones, however, are slightly different and you'll need to implement
    * them. Check Vec.scala or Mat.scala for more details on these operations.
    */
    def -(x: Vec): Vec = n - x
    def -(x: Mat): Mat = n - x

    def /(x: Vec): Vec = n / x
    def /(x: Mat): Mat = n / x
  }
}
