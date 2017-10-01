package task2

import scala.util.{Failure, Success, Try}

object Main {

  private def readIntegers: Iterator[Try[Int]] = Iterator.continually(io.StdIn.readLine)
    .takeWhile(_.nonEmpty)
    .map(s =>Try(s.toInt))
    .map{
      case f @ Failure(_) =>
        println("invalid argument")
        f
      case success => success
    }

  private def readPositiveIntegers: Iterator[Try[Int]] = readIntegers.map{
    case Success(i) if i <= 0 =>
      println("invalid argument")
      Failure(new Exception)
    case t => t
  }

  def main(args: Array[String]): Unit = {

    println("Type positive numbers you wish to be usable in the solution")
    println("Press [Enter] to stop entering inputs and input target")

    val inputs = readPositiveIntegers.flatMap{
      case Success(i) => List(i)
      case Failure(_) => Nil
    }.toList

    println("Enter the target value or press [Enter] to cancel")

    val targetOption = readIntegers.collectFirst{
      case Success(i) => i
    }

    targetOption.foreach{ target =>
      val result = Defs.findExpressionString(inputs,target)
      println(s"for inputs $inputs and target $target:")
      println(result)
    }
  }





}
