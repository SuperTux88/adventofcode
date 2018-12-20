package adventofcode.y2018

object ElfCode {
  val opcodes: Set[String] = Set(
    "addr", "addi",
    "mulr", "muli",
    "banr", "bani",
    "borr", "bori",
    "setr", "seti",
    "gtir", "gtri", "gtrr",
    "eqir", "eqri", "eqrr"
  )

  def execute(opcode: String, params: Vector[Int], reg: Vector[Int]): Vector[Int] = {
    implicit def bool2int(bool: Boolean): Int = if (bool) 1 else 0

    val Seq(a, b, c) = params
    val result: Int = opcode match {
      case "addr" => reg(a) + reg(b)
      case "addi" => reg(a) + b
      case "mulr" => reg(a) * reg(b)
      case "muli" => reg(a) * b
      case "banr" => reg(a) & reg(b)
      case "bani" => reg(a) & b
      case "borr" => reg(a) | reg(b)
      case "bori" => reg(a) | b
      case "setr" => reg(a)
      case "seti" => a
      case "gtir" => a > reg(b)
      case "gtri" => reg(a) > b
      case "gtrr" => reg(a) > reg(b)
      case "eqir" => a == reg(b)
      case "eqri" => reg(a) == b
      case "eqrr" => reg(a) == reg(b)
    }
    reg.updated(c, result)
  }
}
