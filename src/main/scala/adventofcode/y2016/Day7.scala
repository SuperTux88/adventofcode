package adventofcode.y2016

object Day7 extends Year2016 {
  override val day: Int = 7

  val AbbaRE = """.*(\w)((?!\1)\w)(\2)\1.*""".r
  val HypernetRE = """\[|\]""".r

  private val ips = input.getLines.map(parseIP).toList

  printDayPart(1, ips.count(_.supportsTLS))
  printDayPart(2, ips.count(_.supportsSSL))

  private def parseIP(ip: String) = {
    IP(HypernetRE.split(ip).zipWithIndex.map(s => NetSequence(s._1, s._2 % 2 != 0)))
  }

  private case class IP(sequences: Seq[NetSequence]) {
    val (hypernetSeq, networkSeq) = sequences.partition(_.isHypernet)
    def supportsTLS: Boolean = networkSeq.exists(_.hasABBA) && !hypernetSeq.exists(_.hasABBA)
    def supportsSSL: Boolean = {
      val BABs = networkSeq.flatMap(_.getABAs).map(aba => List(aba(1), aba(0), aba(1)).mkString)
      hypernetSeq.exists(seq => BABs.exists(seq.sequence.contains))
    }
  }

  private case class NetSequence(sequence: String, isHypernet: Boolean) {
    def hasABBA: Boolean = AbbaRE.unapplySeq(sequence).isDefined
    def getABAs: Iterator[String] = sequence.sliding(3).filter(s => s(0) == s(2) && s(0) != s(1))
  }
}
