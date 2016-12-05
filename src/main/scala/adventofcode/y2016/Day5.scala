package adventofcode.y2016

import java.security.MessageDigest

import adventofcode.Logging

import scala.annotation.tailrec

object Day5 extends Year2016 {
  override val day: Int = 5

  val doorId = input.mkString

  val first8Chars = (1 to 8).scanLeft((0, 0, 0))((state, _) => nextChar(doorId, state._3)).tail

  printDayPart(1, first8Chars.map(_._1.toHexString).mkString, "password: %s")

  val passwordArray = Array.fill[Option[Char]](8)(None)
  first8Chars.foreach { char =>
    addCharToPassword(passwordArray, char._1, char._2)
    if (Logging.debug) Thread.sleep(500)
  }

  val password = findSecondPassword(passwordArray, doorId, first8Chars.last._3)
  print("\r")
  printDayPart(2, password, "password for second door: %s")

  @tailrec
  private def nextChar(doorId: String, counter: Int): (Int, Int, Int) = {
    val md5 = MessageDigest.getInstance("MD5").digest((doorId + counter).getBytes)
    if (md5(0) == 0 && md5(1) == 0 && (md5(2) & 0xf0) == 0) {
      (md5(2).toInt, md5(3) >> 4, counter + 1)
    } else nextChar(doorId, counter + 1)
  }

  @tailrec
  private def findSecondPassword(password: Array[Option[Char]], doorId: String, counter: Int): String = {
    val (position, char, newCounter) = nextChar(doorId, counter)
    addCharToPassword(password, position, char)

    if (password.exists(_.isEmpty)) findSecondPassword(password, doorId, newCounter)
    else password.flatten.mkString
  }

  private def addCharToPassword(password: Array[Option[Char]], position: Int, char: Int) {
    if (position < password.length && password(position).isEmpty) {
      password(position) = Some(char.toHexString.last)
      printPassword(password)
    }
  }

  private def printPassword(password: Array[Option[Char]]) {
    if (Logging.debug) {
      val string = password.map {
        case None => "_"
        case Some(char) => char
      }.mkString
      print(s"\r$string")
    }
  }
}
