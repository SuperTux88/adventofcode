package adventofcode.y2018

object Day19 extends Year2018 {
  override val day = 19

  val InstructionRE = """(\w+) (\d+) (\d+) (\d+)""".r

  val lines = input.getLines

  val ip = lines.take(1).mkString.split(" ")(1).toInt

  val program = lines.map {
    case InstructionRE(opcode, a, b, c) => (opcode, a.toInt, b.toInt, c.toInt)
  }.toVector

  var registers = Vector.fill(6)(0)

  while(registers(ip) < program.size) { registers = runInstruction(registers) }

  printDayPart(1, registers.head)

  registers = Vector(1, 0, 0, 0, 0, 0)

  while(registers(ip) != 1) { registers = runInstruction(registers) }

  printDayPart(2, factors(registers(1)).sum)

  def runInstruction(registers: Vector[Int]):Vector[Int] = {
    val instruction = program(registers(ip))
    val newRegisters = execute(instruction._1, instruction._2, instruction._3, instruction._4, registers)
    newRegisters.updated(ip, newRegisters(ip) + 1)
  }

  def execute(opcode: String, a: Int, b: Int, c: Int, reg: Vector[Int]): Vector[Int] = {
    implicit def bool2int(bool: Boolean): Int = if (bool) 1 else 0

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

  def factors(num: Int) = {
    (1 to num).filter { divisor =>
      num % divisor == 0
    }
  }
}

/*
0  addi 4 16 4  # GOTO 17
1  seti 1 7 2   # r2 = 1
2  seti 1 1 5   # r5 = 1
3  mulr 2 5 3   # r3 = r2 * r5
4  eqrr 3 1 3   # if (r3 == r1)
5  addr 3 4 4   #   GOTO 7
6  addi 4 1 4   # else GOTO 8
7  addr 2 0 0   # r0 += r2      <-- if (r2 * r5 == r1)
8  addi 5 1 5   # r5 += 1
9  gtrr 5 1 3   # if (r5 > r1)
10 addr 4 3 4   #   GOTO 12
11 seti 2 7 4   # else GOTO 3
12 addi 2 1 2   # r2 += 1
13 gtrr 2 1 3   # if (r2 > r1)
14 addr 3 4 4   #   GOTO 16
15 seti 1 3 4   # else GOTO 2
16 mulr 4 4 4   # GOTO 16*16 (exit)

  var r2 = 1
  var r5 = 1

  while (r2 <= r1) {
    r5 = 1
    while (r5 <= r1) {
      if (r2 * r5 == r1) {
        r0 += r2
      }
      r5 += 1
    }
    r2 += 1
  }

# init value:
17 addi 1 2 1   # r1 = 2
18 mulr 1 1 1   # r1 *= r1
19 mulr 4 1 1   # r1 *= 19
20 muli 1 11 1  # r1 *= 11
21 addi 3 3 3   # r3 = 3
22 mulr 3 4 3   # r3 *= 22
23 addi 3 9 3   # r3 += 9
24 addr 1 3 1   # r1 += r3
25 addr 4 0 4   # if (part 1)
26 seti 0 1 4   # GOTO 1 else GOTO 27
27 setr 4 9 3   # r3 = 27
28 mulr 3 4 3   # r3 *= 28
29 addr 4 3 3   # r3 += 29
30 mulr 4 3 3   # r3 *= 30
31 muli 3 14 3  # r3 *= 14
32 mulr 3 4 3   # r3 *= 32
33 addr 1 3 1   # r1 += r3
34 seti 0 6 0   # r0 = 0
35 seti 0 7 4   # GOTO 1

init value:
part 1: r1 = 911
part 2: r1 = 911 + 10550400
 */
