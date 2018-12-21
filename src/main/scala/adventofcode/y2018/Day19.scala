package adventofcode.y2018

import scala.annotation.tailrec

object Day19 extends Year2018 {
  override val day = 19

  val elfCode = new ElfCode(input.getLines)

  val registerWithNumberToFactor = 1 // TODO get this from the input

  printDayPart(1, elfCode.runProgram().head)
  printDayPart(2, factors(findNumberToFactor(Vector(1, 0, 0, 0, 0, 0))).sum)

  @tailrec
  final def findNumberToFactor(registers: Vector[Int]): Int = {
    if (registers(elfCode.ip) != 1)
      findNumberToFactor(elfCode.runInstruction(registers))
    else
      registers(registerWithNumberToFactor)
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
