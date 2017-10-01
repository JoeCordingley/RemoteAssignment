/**
  * Created by joe on 30/09/17.
  */
object Task1 {

  def lcs(left:String, right:String):String = {
    def go(left:List[Char],right:List[Char]):List[Char] = (left,right) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (l::ls,r::rs) if l == r => l :: go(ls,rs)
      case (_::ls,_::rs) =>
        val dropLeft = go(ls,right)
        val dropRight = go(left,rs)
        if (dropLeft.length >= dropRight.length) dropLeft else dropRight
    }
    new String(go(left.toList,right.toList).toArray)
  }

  //Stack safe version
  def lcs2(left:String, right:String):String = {
    def go(left:List[Char],right:List[Char]):Stream[Char] = (left,right) match {
      case (Nil,_) => Stream.empty
      case (_,Nil) => Stream.empty
      case (l::ls,r::rs) if l == r => l #:: go(ls,rs)
      case (_::ls,_::rs) =>
        val dropLeft = go(ls,right)
        val dropRight = go(left,rs)
        if (dropLeft.length >= dropRight.length) dropLeft else dropRight
    }
    new String(go(left.toList,right.toList).toArray)
  }


}
