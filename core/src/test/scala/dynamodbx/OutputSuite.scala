package dynamodbx

import cats.tests.CatsSuite

class OutputSuite extends CatsSuite {

  case class Version(n: Int)
  case class Foo(s: Version, i: Int)

  implicit val versionOutput: Output[Version] = Output[String].map(s => Version(s.split('#').last.toInt))

  test("stuff") {

    val f = Read[Foo].unsafeRead(List(S("Version#000004"), N(123)), 0)
    f shouldEqual Foo(Version(4), 123)

  }

}
