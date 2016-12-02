package adventofcode.y2015

import java.security.MessageDigest
import java.util.concurrent.LinkedBlockingQueue

import scala.compat.Platform._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object Day4 extends Year2015 {
  override val day: Int = 4

  val inputStr = input.mkString

  //calculate(5)
  //calculate(6)
  printDayPart(1, calculateParallel(5))
  printDayPart(2, calculateParallel(6))

  private def calculate(numberOfZeroes: Int): Int = {
    implicit val expectedResult = ExpectedResult(numberOfZeroes)

    var counter = 0
    def nextCounter = { counter += 1; counter }

    val start = currentTime

    while(!calculateStringAndMatch(inputStr + nextCounter)) {}

    printDebug(s"with $numberOfZeroes zeros | calculated in ${currentTime - start}ms")
    counter
  }

  private def calculateParallel(numberOfZeroes: Int): Int = {
    implicit val expectedResult = ExpectedResult(numberOfZeroes)
    val batchSize = 5000

    var counter = 0
    def nextBatch = this.synchronized { val oldCounter = counter; counter += batchSize; oldCounter to counter }
    var result: List[Int] = Nil
    val queue = new LinkedBlockingQueue[Future[Int]](15)

    val start = currentTime

    import ExecutionContext.Implicits.global
    do {
      val future = Future[Int] {
        calculateFirstInBatch(nextBatch, batchSize)
      }
      future.foreach {
        case -1 => queue.remove(future)
        case value: Int =>
          result = value :: result
          queue.remove(future)
      }
      queue.put(future)
    } while(result.isEmpty)

    val first = currentTime

    import scala.language.postfixOps
    while(!queue.isEmpty) { Await.result(queue.poll, 2 seconds) }

    printDebug(s"with $numberOfZeroes zeros | first result after in ${first - start}ms | finished after ${currentTime - start}ms")
    result.sorted.head
  }

  private def calculateFirstInBatch(batch: Range, batchSize: Int)(implicit expectedResult: ExpectedResult): Int = {
    batch.find( counter => calculateStringAndMatch(inputStr + counter)).getOrElse(-1)
  }

  private def calculateStringAndMatch(string: String)(implicit expectedResult: ExpectedResult): Boolean = {
    val md5 = MessageDigest.getInstance("MD5").digest(string.getBytes)
    md5.slice(0, expectedResult.numberOfBytes).forall(_ == 0) &&
      (expectedResult.numberOfZeroes % 2 == 0 || (md5(expectedResult.numberOfBytes) & 0xf0) == 0)
  }

  private case class ExpectedResult(numberOfZeroes: Int, numberOfBytes: Int)
  private implicit object ExpectedResult {
    def apply(numberOfZeroes: Int): ExpectedResult = ExpectedResult(numberOfZeroes, numberOfZeroes/2)
  }
}
