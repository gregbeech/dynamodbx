package dynamodbx

import cats.tests.CatsSuite

class OutputSuite extends CatsSuite {

  case class Version(n: Int)
  case class Foo(id: Int, version: Version)

  implicit val versionOutput: Output[Version] = Output[String].map(s => Version(s.split('#').last.toInt))

  test("unsafeRead") {
    val names = AttrNames[Foo].names
    val foo = Read[Foo].unsafeRead(Map("Id" -> N(123), "Version" -> S("Version#000004")), names)
    foo shouldEqual Foo(123, Version(4))
  }

}
