import task2.Defs.{Expression, Minus, Number, Plus, Times}
import org.scalatest.{FreeSpec, Matchers}
import task2.Defs

import scala.util.Random

/**
  * Created by joe on 30/09/17.
  */
class AssignmentTests extends FreeSpec with Matchers{

  "Task1 inputs AABACDA and DACBBCAD should return ABCA" in {
    Task1.lcs("AABACDA","DACBBCAD") should equal("ABCA")
  }

  "Task1 stack safety test" in {
    val rand = new Random(System.currentTimeMillis())
    val chars = Vector('A', 'B', 'C', 'D')
    val lengths = 10000
    val randomStream = Stream.continually(rand.nextInt(chars.length)).map(chars(_)).take(lengths * 2).toList
    val (randomChars,randomChars2) = randomStream.splitAt(lengths)
    Task1.lcs2(randomChars.mkString,randomChars.mkString)

  }

  "Task2 input List(2,3,5,6) with target 42 should return a valid output" in {
    val ret = Defs.findExpression(List(2,3,5,6),42)
    ret shouldBe defined
    Defs.evaluate(ret.get) should equal(42)
  }

  case class EvaluateTest(inputs: List[Int], expressions: Map[Expression,String],result:Int)
  val  evaluateTests =  List(
    EvaluateTest(
      inputs = List(1,2,3),
      expressions = Map(
        Plus(1,Times(2,3)) -> "(1 + 2 * 3)",
        Plus(1, Times(3,2)) -> "(1 + 3 * 2)",
        Plus(Times(2,3),1) -> "(2 * 3 + 1)",
        Plus(Times(3,2),1) -> "(3 * 2 + 1)"
      ),
      result = 7
    ),
    EvaluateTest(
      inputs = List(2,2,3,4),
      expressions = Map(
        Minus(Times(2,4),Times(2,3)) -> "(2 * 4 - 2 * 3)",
        Minus(Times(4,2),Times(2,3)) -> "(4 * 2 - 2 * 3)",
        Minus(Times(2,4),Times(3,2)) -> "(2 * 4 - 3 * 2)",
        Minus(Times(4,2),Times(3,2)) -> "(4 * 2 - 3 * 2)"
      ),
      result = 2
    ),
    EvaluateTest(
      inputs = List(2,3,4),
      expressions = Map(
        Times(3,Minus(2,4)) -> "3 * (2 - 4)",
        Times(Minus(2,4),3) -> "(2 - 4) * 3"
      ),
      result = -6
    )
  )

  "evaluate" - {
    for {
      EvaluateTest(_,expressions,result) <- evaluateTests
      expression <- expressions.keys
    }{
      s"$expression should equal $result" in {
        Defs.evaluate(expression) should equal(result)
      }
    }

  }

  "getAllPossibleExpressions" - {
    for(EvaluateTest(inputs,expressions,_) <- evaluateTests){
      s"for inputs $inputs should include one of ${expressions.keys}" in {
        Defs.getAllPossibleExpressions(inputs) should contain oneElementOf expressions.keys
      }
      s"for inputs $inputs should not repeat solutions" in {
        val ret = Defs.getAllPossibleExpressions(inputs)
        val numberOfSolutions = ret.length
        ret.toSet should have size numberOfSolutions
      }
    }
  }

  "findExpression" - {
    for (EvaluateTest(inputs,_,target) <- evaluateTests) {
      s"should find a valid expression for inputs $inputs and target $target" in {
        val ret = Defs.findExpression(inputs,target)
        ret shouldBe defined
        Defs.evaluate(ret.get) should equal(target)

      }
    }
    "should find no valid expression when there isn't one available" in {
      Defs.findExpression(List(1,2,4),30) shouldBe None
    }
    "should behave correctly with no inputs" in {
      Defs.findExpression(Nil,0) shouldBe None
    }
  }

  "expressionToString" -{
    for {
      EvaluateTest(_,expressions,_) <- evaluateTests
      (expression,string) <- expressions
    } {
      s"""for expression $expression should equal "$string" """ in {
        Defs.expressionToString(expression) should equal(string)
      }
    }
  }

}
