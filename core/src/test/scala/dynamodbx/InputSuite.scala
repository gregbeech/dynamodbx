package dynamodbx

import cats.tests.CatsSuite

class InputSuite extends CatsSuite {

  test("Boolean -> BOOL") {
    forAll { f: Boolean => Input[Boolean].value(f) shouldEqual BOOL(f) }
  }
  test("Double -> N") {
    forAll { n: Double => Input[Double].value(n) shouldEqual N(n.toString) }
  }
  test("Int -> N") {
    forAll { n: Int => Input[Int].value(n) shouldEqual N(n.toString) }
  }
  test("List -> L") {
    forAll { xs: List[Int] => Input[List[Int]].value(xs) shouldEqual L(xs.map(N(_))) }
  }
  test("Set[Double] -> NS") {
    forAll { xs: Set[Double] => Input[Set[Double]].value(xs) shouldEqual NS(xs) }
  }
  test("Set[Int] -> NS") {
    forAll { xs: Set[Int] => Input[Set[Int]].value(xs) shouldEqual NS(xs) }
  }
  test("Set[String] -> SS") {
    forAll { xs: Set[String] => Input[Set[String]].value(xs) shouldEqual SS(xs) }
  }

}
