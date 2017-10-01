package task2

import scala.util.{Failure, Success, Try}

/**
  * Created by joe on 30/09/17.
  */
object Defs {

  sealed trait Expression
  sealed trait BinaryExpression extends Expression
  case class Number(i:Int) extends Expression
  case class Plus(left:Expression,right: Expression) extends BinaryExpression
  case class Times(left:Expression, right: Expression) extends BinaryExpression
  case class Minus(left:Expression, right: Expression) extends BinaryExpression

  def expressionToString(expression: Expression) : String = expression match {
    case Number(i) => i.toString
    case Plus(left,right) => s"(${expressionToString(left)} + ${expressionToString(right)})"
    case Minus(left,right) => s"(${expressionToString(left)} - ${expressionToString(right)})"
    case Times(left,right) => s"${expressionToString(left)} * ${expressionToString(right)}"
  }

  def evaluate(expression: Expression):Int = expression match {
    case Number(i) => i
    case Plus(left,right) => evaluate(left) + evaluate(right)
    case Times(left,right) => evaluate(left) * evaluate(right)
    case Minus(left,right) => evaluate(left) - evaluate(right)
  }

  implicit def intToNumber(i:Int):Number = Number(i)

  val binaryExpressions: List[(Expression,Expression)=> BinaryExpression] = List(
    Plus.apply _,
    Times.apply _,
    Minus.apply _
  )

  private def getAllExpressionCombinations(numbers:List[Int]):Stream[Expression] = numbers match {
    case List(x) => Stream(Number(x))
    case _ => for {
      splitPoint <- (1 until numbers.length).toStream
      (left,right) = numbers.splitAt(splitPoint)
      leftExpression <- getAllExpressionCombinations(left)
      rightExpression <- getAllExpressionCombinations(right)
      expression <- binaryExpressions
    } yield expression(leftExpression,rightExpression)
  }

  def getAllPossibleExpressions(numbers:List[Int]):Stream[Expression] = for {
    length <- (1 to numbers.length).toStream
    combination <- numbers.combinations(length)
    permutation <- combination.permutations
    expression <- getAllExpressionCombinations(permutation)
  } yield expression

  def findExpression(numbers:List[Int], target:Int):Option[Expression] = getAllPossibleExpressions(numbers)
    .find(expression => evaluate(expression) == target)

  val noSolutionsMessage = "no solutions exist"

  def findExpressionString(numbers:List[Int],target:Int):String = findExpression(numbers,target) match {
    case Some(expression) => {
      val solution = expressionToString(expression)
      s"a solution is $solution"
    }
    case None => noSolutionsMessage
  }

}
