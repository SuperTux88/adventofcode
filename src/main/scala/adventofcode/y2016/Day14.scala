package adventofcode.y2016

import java.security.MessageDigest

import adventofcode.Logging

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 extends Year2016 {
  override val day = 14

  private val ThreeCharsRE = """(\w)\1{2}""".r
  private val hexChars = "0123456789abcdef".getBytes

  override def runDay(salt: String): Unit = {
    implicit val cachedMd5: CachedMD5 = new CachedMD5(salt)

    printDayPart(1, getKeyIndex())

    cachedMd5.clear()
    printDayPart(2, getKeyIndex(stretching = true))
  }

  private def getKeyIndex(stretching: Boolean = false)(implicit cachedMd5: CachedMD5) =
    (1 to 64).foldLeft(0)((counter, _) => nextKeyIndex(counter + 1, stretching))

  @tailrec
  private def nextKeyIndex(counter: Int, stretching: Boolean)(implicit cachedMd5: CachedMD5): Int = {
    val md5String = cachedMd5.get(counter, stretching)
    cachedMd5.remove(counter)

    ThreeCharsRE.findFirstMatchIn(md5String) match {
      case Some(matched) =>
        val searched = matched.group(1) * 5
        (counter + 1 to counter + 1000).map(cachedMd5.get(_, stretching)).find(_.contains(searched)) match {
          case Some(found) =>
            if (Logging.debug) println(s"$counter | $md5String | $found")
            counter
          case None => nextKeyIndex(counter + 1, stretching)
        }
      case None => nextKeyIndex(counter + 1, stretching)
    }
  }

  private def getHexBytes(bytes: Array[Byte]) = bytes.flatMap { byte =>
    val b = byte & 0xff
    Array(hexChars(b / 16), hexChars(b % 16))
  }

  private class CachedMD5(salt: String) {
    private val md5Cache = mutable.Map.empty[Int, String]

    def get(counter: Int, stretching: Boolean): String =
      md5Cache.getOrElseUpdate(counter, md5((salt + counter).getBytes, stretching))

    def remove(counter: Int): Unit = md5Cache.remove(counter)

    def clear(): Unit = md5Cache.clear()

    private def md5(bytes: Array[Byte], stretching: Boolean): String = {
      val md = MessageDigest.getInstance("MD5")
      var digest = md.digest(bytes)
      if (stretching) (1 to 2016).foreach(_ => digest = md.digest(getHexBytes(digest)))
      getHexBytes(digest).map(_.toChar).mkString
    }
  }
}
