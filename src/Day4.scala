import java.security.MessageDigest
import java.util.concurrent.LinkedBlockingQueue

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object Day4 extends App {
  val input = Input(4).string

  calculate(5)
  calculate(6)
  calculateParallel(5)
  calculateParallel(6)

  // second run after JIT
  calculate(5)
  calculate(6)
  calculateParallel(5)
  calculateParallel(6)

  private def calculate(numberOfZeroes: Int) = {
    implicit val expectedResult = ExpectedResult(numberOfZeroes)

    var counter = 0
    def nextCounter = { counter += 1; counter }

    val start = System.currentTimeMillis

    while(!calculateStringAndMatch(input + nextCounter)) {}

    val end = System.currentTimeMillis

    println(s"first number with $numberOfZeroes zeroes: $counter | calculated in ${end - start}ms")
  }

  private def calculateParallel(numberOfZeroes: Int) = {
    implicit val expectedResult = ExpectedResult(numberOfZeroes)
    val batchSize = 5000

    var counter = 0
    def nextOffset = this.synchronized { val ret = counter; counter += batchSize; ret }
    var result: List[Int] = Nil
    val queue = new LinkedBlockingQueue[Future[Int]](15)

    val start = System.currentTimeMillis

    import ExecutionContext.Implicits.global
    do {
      val future = Future[Int] {
        calculatefirstInBatch(nextOffset, batchSize)
      }
      future.onSuccess {
        case -1 => queue.remove(future)
        case value: Int =>
          result = value :: result
          queue.remove(future)
      }
      queue.put(future)
    } while(result.isEmpty)

    val first = System.currentTimeMillis

    while(!queue.isEmpty) { Await.result(queue.poll, 2 seconds) }

    val end = System.currentTimeMillis

    println(s"first number with $numberOfZeroes zeroes: ${result.sorted.head} | first result after in ${first - start}ms | finished after ${end - start}ms")
  }

  private def calculatefirstInBatch(offset: Int, batchSize: Int)(implicit expectedResult: ExpectedResult): Int = {
    (offset to offset+batchSize).find( counter => calculateStringAndMatch(input + counter)).getOrElse(-1)
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
