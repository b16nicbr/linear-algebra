import org.scalatest.flatspec.AnyFlatSpec
import linalg._

class SampleTests extends AnyFlatSpec {
  val v1: Vec = Vec(1, 1, 1)
  val v2: Vec = Vec(2, 2, 2)
  val v3: Vec = Vec(1, 2, 3)
  val v4: Vec = Vec(4, 4, 4)

  val m1: Mat = Mat(v1, v1, v1)
  val m2: Mat = Mat(v2, v2, v2)
  val m3: Mat = Mat(v1, v2, v3)
  val m4: Mat = Mat(v4, v4, v4)

  info("======= Selecting and slicing ======")

  info("---- Vectors ----")
  "v3(0)" should "be equal to 1" in {
    assert(v3(0) == 1)
  }

  "v3(2)" should "be equal to 3" in {
    assert(v3(2) == 3)
  }

  "v3(1 to 2)" should "be equal to Vec(2, 3)" in {
    assert(v3(1 to 2) equals Vec(2, 3))
  }

  info("---- Matrices ----")
  "m3(1, 0)" should "be equal to v2" in {
    assert(m3(1, 0) equals v2)
  }

  "m3(2, 1)" should "be equal to Vec(1, 2, 3)" in {
    assert(m3(2, 1) equals Vec(1, 2, 3))
  }

  "m3(0 to 1, 0)" should "be equal to Mat(v1, v2)" in {
    assert(m3(0 to 1, 0) equals Mat(v1, v2))
  }

  "m3(1 to 2, 1)" should "be equal to Mat([1, 1], [2, 2], [2, 3])" in {
    assert(m3(1 to 2, 1) equals Mat(Vec(1, 1), Vec(2, 2), Vec(2, 3)))
  }

  info("======= Arithmetic operations ======")

  info("---- Addition ----")
  "v1 + 1" should "be equal to Vec(2, 2, 2)" in {
    assert(v1 + 1 equals Vec(2, 2, 2))
  }

  "1 + v1" should "be equal to Vec(2, 2, 2)" in {
    assert(1 + v1 equals Vec(2, 2, 2))
  }

  "v1 + v1" should "be equal to Vec(2, 2, 2)" in {
    assert(v1 + v1 equals Vec(2, 2, 2))
  }

  "v1 + v2" should "be equal to Vec(3, 3, 3)" in {
    assert(v1 + v2 equals Vec(3, 3, 3))
  }

  "v1 + m1" should "be equal to m2" in {
    assert(v1 + m1 equals m2)
  }

  "m1 + v1" should "be equal to m2" in {
    assert(m1 + v1 equals m2)
  }

  "m1 + m1" should "be equal to m2" in {
    assert(m1 + m1 equals m2)
  }

  info("---- Multiplication ----")
  "v2 * 2" should "be equal to Vec(4, 4, 4)" in {
    assert(v2 * 2 equals Vec(4, 4, 4))
  }

  "2 * v2" should "be equal to Vec(4, 4, 4)" in {
    assert(2 * v2 equals Vec(4, 4, 4))
  }

  "v2 * v2" should "be equal to v4" in {
    assert(v2 * v2 equals v4)
  }

  "v2 * m2" should "be equal to m4" in {
    assert(v2 * m2 equals m4)
  }

  "m2 * m2" should "be equal to m4" in {
    assert(m2 * m2 equals m4)
  }

  info("---- Substraction ----")
  "v1 - v1" should "be equal to Vec(0, 0, 0)" in {
    assert(v1 - v1 equals Vec(0, 0, 0))
  }

  "v2 - v1" should "be equal to Vec(1, 1, 1)" in {
    assert(v2 - v1 equals Vec(1, 1, 1))
  }

  "v1 - v2" should "be equal to Vec(-1, -1, -1)" in {
    assert(v1 - v2 equals Vec(-1, -1, -1))
  }

  info("---- Division ----")
  "v2 / v1" should "be equal to Vec(2, 2, 2)" in {
    assert(v2 / v1 equals Vec(2, 2, 2))
  }

  "v1 / v2" should "be equal to Vec(.5, .5, .5)" in {
    assert(v1 / v2 equals Vec(.5, .5, .5))
  }

  info("======= dot product ======")
  info("---- Vectors ----")
  "v1 dot v1" should "be equal to 3" in {
    assert((v1 dot v1) == 3)
  }

  "v2 dot v3" should "be equal to 12" in {
    assert((v2 dot v3) == 12)
  }
}
