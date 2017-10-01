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

  case class EvaluateTest(inputs: List[Int], expression: Expression,result:Int, string: String)
  val  evaluateTests =  List(
    EvaluateTest(inputs = List(1,2,3), expression = Plus(1,Times(2,3)),result = 7, string = "(1 + 2 * 3)"),
    EvaluateTest(inputs = List(2,2,3,4),expression = Minus(Times(2,4),Times(2,3)),result = 2, string = "(2 * 4 - 2 * 3)"),
    EvaluateTest(inputs = List(2,3,4),expression = Times(3,Minus(2,4)),result = -6, string = "3 * (2 - 4)")
  )

  "evaluate" - {
    for ( EvaluateTest(_,expression,result,_) <- evaluateTests ){
      s"$expression should equal $result" in {
        Defs.evaluate(expression) should equal(result)
      }
    }

  }

  "getAllPossibleExpressions" - {
    for(EvaluateTest(inputs,expression,_,_) <- evaluateTests){
      s"for inputs $inputs should include $expression" in {
        Defs.getAllPossibleExpressions(inputs) should contain(expression)
      }
      s"for inputs $inputs should not repeat solutions" in {
        val ret = Defs.getAllPossibleExpressions(inputs)
        val numberOfSolutions = ret.length
        ret.toSet should have size numberOfSolutions
      }
    }
  }

  "findExpression" - {
    for (EvaluateTest(inputs,_,target,_) <- evaluateTests) {
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
    for (EvaluateTest(_,expression,_,string) <- evaluateTests) {
      s"""for expression $expression should equal "$string" """ in {
        Defs.expressionToString(expression) should equal(string)
      }
    }
  }

}
