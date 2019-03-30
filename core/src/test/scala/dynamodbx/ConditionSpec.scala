package dynamodbx

import org.scalatest.{MustMatchers, WordSpec}

class ConditionSpec extends WordSpec with MustMatchers {

  "stuff" in {
    PK("foo.bar").eql("stuff") and SK("bar[0]").eql(true) mustEqual KeyCondition(
      PKCondition(Path(Name("foo"), List(Name("bar"))), Eq(S("stuff"))),
      SKCondition(Path(Name("bar"), List(ListIndex(0))), Eq(BOOL(true)))
    )
  }

}
