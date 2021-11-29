package adventofcode.y2020

import adventofcode.common.NumberHelper.isInRange

import scala.io.BufferedSource

object Day2 extends Year2020 {
  override val day = 2

  private val PasswordRE = """(\d+)-(\d+) (\w): (\w+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val passwords = input.getLines().map {
      case PasswordRE(a, b, char, password) =>
        Password(a.toInt, b.toInt, char.charAt(0), password)
    }.toSeq

    printDayPart(1, passwords.count(_.isValid), "valid passwords: %s")
    printDayPart(2, passwords.count(_.isValidTobogganCorporatePolicy),
      "valid passwords according to according to the new interpretation of the policies: %s")
  }

  private case class Password(a: Int, b: Int, char: Char, password: String) {
    def isValid: Boolean =
      isInRange(password.count(_ == char), a, b)

    def isValidTobogganCorporatePolicy: Boolean =
      hasCharAtPos(char, a - 1) ^ hasCharAtPos(char, b - 1)

    private def hasCharAtPos(char: Char, pos: Int) =
      password.length > pos && password.charAt(pos) == char
  }
}
