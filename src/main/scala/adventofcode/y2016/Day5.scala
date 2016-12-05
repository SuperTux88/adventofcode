package adventofcode.y2016

import java.security.MessageDigest

import adventofcode.Logging

import scala.annotation.tailrec
import scala.util.Random

object Day5 extends Year2016 {
  override val day: Int = 5

  val doorId = input.mkString

  val passwordArray = Array.fill[Option[Char]](8)(None)
  val passwordArray2 = Array.fill[Option[Char]](8)(None)

  val first8Chars = (0 to 7).scanLeft((0, 0, 0)) { (state, position) =>
    val char = nextChar(doorId, state._3, passwordArray)
    addCharToPassword(passwordArray, position, char._1)
    char
  }.tail

  print("\r")
  printDayPart(1, first8Chars.map(_._1.toHexString).mkString, "password: %s")

  first8Chars.foreach { char =>
    addCharToPassword(passwordArray2, char._1, char._2)
    if (Logging.results) (1 to 15).foreach { _ =>
      printPassword(passwordArray2)
      Thread.sleep(40)
    }
  }

  val password2 = findSecondPassword(passwordArray2, doorId, first8Chars.last._3)
  print("\r")
  printDayPart(2, password2, "password for second door: %s")

  @tailrec
  private def nextChar(doorId: String, counter: Int, password: Array[Option[Char]]): (Int, Int, Int) = {
    val md5 = MessageDigest.getInstance("MD5").digest((doorId + counter).getBytes)

    if (Logging.results && counter % 50000 == 0) printPassword(password)

    if (md5(0) == 0 && md5(1) == 0 && (md5(2) & 0xf0) == 0) {
      (md5(2).toInt, md5(3) >> 4, counter + 1)
    } else nextChar(doorId, counter + 1, password)
  }

  @tailrec
  private def findSecondPassword(password: Array[Option[Char]], doorId: String, counter: Int): String = {
    val (position, char, newCounter) = nextChar(doorId, counter, password)
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
    if (Logging.results) {
      val string = password.map {
        case None => Random.nextInt(15).toHexString
        case Some(char) => char
      }.mkString
      print(s"\r$string")
    }
  }
}
