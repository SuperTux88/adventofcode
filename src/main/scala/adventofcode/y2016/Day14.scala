package adventofcode.y2016

import java.security.MessageDigest

import adventofcode.Logging

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 extends Year2016 {
  override val day = 14

  val ThreeCharsRE = """(\w)\1{2}""".r
  val hexChars = "0123456789abcdef".getBytes

  val salt = inputString

  val md5Cache = mutable.Map.empty[Int, String]

  printDayPart(1, getKeyIndex(salt))

  md5Cache.clear()
  printDayPart(2, getKeyIndex(salt, stretching = true))

  private def getKeyIndex(salt: String, stretching: Boolean = false) =
    (1 to 64).foldLeft(0)((counter, _) => nextKeyIndex(salt, counter + 1, stretching))

  @tailrec
  private def nextKeyIndex(salt: String, counter: Int, stretching: Boolean): Int = {
    val md5String = cachedMd5(salt, counter, stretching)
    md5Cache.remove(counter)

    ThreeCharsRE.findFirstMatchIn(md5String) match {
      case Some(matched) =>
        val searched = matched.group(1) * 5
        (counter + 1 to counter + 1000).map(cachedMd5(salt, _, stretching)).find(_.contains(searched)) match {
          case Some(found) =>
            if (Logging.debug) println(s"$counter | $md5String | $found")
            counter
          case None => nextKeyIndex(salt, counter + 1, stretching)
        }
      case None => nextKeyIndex(salt, counter + 1, stretching)
    }
  }

  private def cachedMd5(salt: String, counter: Int, stretching: Boolean) =
    md5Cache.getOrElseUpdate(counter, md5((salt + counter).getBytes, stretching))

  private def md5(bytes: Array[Byte], stretching: Boolean): String = {
    val md = MessageDigest.getInstance("MD5")
    var digest = md.digest(bytes)
    if (stretching) (1 to 2016).foreach(_ => digest = md.digest(getHexBytes(digest)))
    getHexBytes(digest).map(_.toChar).mkString
  }

  private def getHexBytes(bytes: Array[Byte]) = bytes.flatMap { byte =>
    val b = byte & 0xff
    Array(hexChars(b / 16), hexChars(b % 16))
  }
}
