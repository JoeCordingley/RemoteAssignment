package task2

import scala.util.{Failure, Success, Try}

/**
  * Created by joe on 30/09/17.
  */
object Defs {

  sealed trait Expression
  sealed trait BinaryExpressionObj {
    def apply(left: Expression, right: Expression): Expression
  }
  sealed trait Commutative
  case class Number(i:Int) extends Expression
  case class Plus(left:Expression,right: Expression) extends Expression
  case class Times(left:Expression, right: Expression) extends Expression
  case class Minus(left:Expression, right: Expression) extends Expression

  object Plus extends BinaryExpressionObj with Commutative
  object Times extends BinaryExpressionObj with Commutative
  object Minus extends BinaryExpressionObj

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

  val binaryExpressions: List[BinaryExpressionObj] = List(
    Plus,
    Times,
    Minus
  )

  private def getAllExpressionCombinations(numbers:List[Int]):Stream[Expression] = numbers match {
    case List(x) => Stream(Number(x))
    case _ => for {
      (left,right) <- splitCombinations(numbers)
      leftExpression <- getAllExpressionCombinations(left)
      rightExpression <- getAllExpressionCombinations(right)
      expression <- binaryExpressions
      (newLeft,newRight) <- expression match {
        case _: Commutative => singlePermutation(leftExpression,rightExpression)
        case _ => bothPermutations(leftExpression,rightExpression)
      }
    } yield expression(newLeft,newRight)
  }

  private def singlePermutation[A](l:A,r:A):Set[(A,A)] = Set((l,r))
  private def bothPermutations[A](l:A,r:A):Set[(A,A)] = Set((l,r),(r,l))

  //TODO Test this, perhaps using scalacheck
  //returns all the possible ways of splitting a list into two groups, but with no symmetry
  private def splitCombinations[A](as: List[A]): Stream[(List[A], List[A])] = {
    val leftCombinations = (1 until as.length).toStream.flatMap(as.combinations)
    val asReverse = as.reverse
    val length = ((1 until as.length).map(as.combinations).map(_.length).sum+1)/2
    val rightCombinations = (1 until as.length).reverse.toStream.flatMap(asReverse.combinations)
    leftCombinations.zip(rightCombinations).take(length)
  }
  def getAllPossibleExpressions(numbers:List[Int]):Stream[Expression] = for {
    length <- (1 to numbers.length).toStream
    combination <- numbers.combinations(length)
    expression <- getAllExpressionCombinations(combination)
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
