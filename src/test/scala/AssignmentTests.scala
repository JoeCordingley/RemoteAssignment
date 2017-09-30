import org.scalatest.{FreeSpec, Matchers}

/**
  * Created by joe on 30/09/17.
  */
class AssignmentTests extends FreeSpec with Matchers{

  "Task1 inputs AABACDA and DACBBCAD should return ABCA" in {
    Task1.lcs("AABACDA","DACBBCAD") should equal("ABCA")

  }
  "Task2 input List(2,3,5,6) with target 42 should return a valid output" in {
    val ret = Task2.countDown(List(2,3,5,6),42)
    ret shouldBe defined
    Task2.evaluate(ret.get) should equal(42)
  }


}
