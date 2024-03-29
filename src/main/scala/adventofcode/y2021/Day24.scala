package adventofcode.y2021

import scala.annotation.tailrec

object Day24 extends Year2021 {
  override val day = 24

  private val InpInstrRE =
    """mul x 0
      |add x z
      |mod x 26
      |div z (1|26)
      |add x (-?\d+)
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y (-?\d+)
      |mul y x
      |add z y""".stripMargin.r

  override def runDay(input: String): Unit = {
    val inputVariables = input.split("inp w\n").map(_.trim).dropWhile(_.isEmpty).zipWithIndex.map {
      case (InpInstrRE(divZ, addX, addY), index) => InputVariables(index, divZ == "26", addX.toInt, addY.toInt)
    }.toList

    val offsets = getPairs(inputVariables).flatMap(getDiff).sortBy(_._1).map(_._2)

    printDayPart(1, getNumber(offsets), "largest model number accepted by MONAD: %s")
    printDayPart(2, getNumber(offsets, false), "smallest model number accepted by MONAD: %s")
  }

  private def getNumber(offsets: Seq[Int], largest: Boolean = true) =
    if (largest)
      offsets.map(diff => if diff < 0 then 9 + diff else 9).mkString
    else
      offsets.map(diff => if diff > 0 then 1 + diff else 1).mkString

  @tailrec
  private def getPairs(instructions: List[InputVariables],
                       left: List[InputVariables] = Nil,
                       pairs: List[(InputVariables, InputVariables)] = Nil
                      ): List[(InputVariables, InputVariables)] =
    instructions match {
      case mul :: div :: rest if !mul.divZ && div.divZ => getPairs(left.reverse ::: rest, Nil, (mul, div) :: pairs)
      case head :: tail => getPairs(tail, head :: left, pairs)
      case Nil => pairs
    }

  // difference between first and second number in mul/div pair
  private def getDiff(pair: (InputVariables, InputVariables)) = {
    val (mul, div) = pair
    // (input + mul.addY) => z
    // (z     + div.addX) == input
    val diff = mul.addY + div.addX
    Map(mul.index -> (diff * -1), div.index -> diff)
  }

  private case class InputVariables(index: Int, divZ: Boolean, addX: Int, addY: Int)
}
