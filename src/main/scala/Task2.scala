
/**
  * Created by joe on 30/09/17.
  */
object Task2 {

  sealed trait Expression
  case class Number(i:Int) extends Expression
  case class Plus(left:Expression,right: Expression) extends Expression
  case class Times(left:Expression, right: Expression) extends Expression
  case class Minus(left:Expression, right: Expression) extends Expression
  def evaluate(expression: Expression):Int = expression match {
    case Number(i) => i
    case Plus(left,right) => evaluate(left) + evaluate(right)
    case Times(left,right) => evaluate(left) * evaluate(right)
    case Minus(left,right) => evaluate(left) - evaluate(right)
  }
  def countDown(numbers:List[Int], target:Int):Option[Expression] = {
    type ExpressionFunction = (Int,Expression) => Expression
    val leaveHeadOff : ExpressionFunction = (_,expression) => expression
    val putInPlus: ExpressionFunction = (x,expression) => Plus(Number(x),expression)
    val putInTimes: ExpressionFunction = (x,expression) => Times(Number(x),expression)
    val putInLeftHandSideOfMinus: ExpressionFunction = (x,expression) => Minus(Number(x),expression)
    val putInRightHandSideOfMinus: ExpressionFunction = (x,expression) => Minus(expression,Number(x))
    val functions : List[ExpressionFunction] = List(
      leaveHeadOff,
      putInPlus,
      putInRightHandSideOfMinus,
      putInTimes,
      putInLeftHandSideOfMinus
    )
    def getAllPossibleExpressions(numbers:List[Int]):Stream[Expression] = numbers match {
      case Nil => Stream.empty
      case x :: xs => for {
        function <- functions.toStream
        expression <- getAllPossibleExpressions(xs)
      } yield function(x,expression)
    }
    numbers match {
      case Nil => None
      case x :: _ if x == target => Some(Number(x))
      case x :: xs => {
        val stream = countDown(xs,target) #:: plus(x,xs) #:: rightOfMinus(x,xs) #:: times(x,xs) #:: leftOfMinus(x,xs) #:: Stream.empty
        stream.find(_.isDefined).flatten
      }
    }
  }

}
