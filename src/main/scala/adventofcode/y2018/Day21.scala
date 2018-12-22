package adventofcode.y2018

object Day21 extends Year2018 {
  override val day = 21

  val elfCode = new ElfCode(input.getLines)

  val instructionWithHaltCheck = elfCode.size - 3
  val registerWithNumberForExit = elfCode.program(instructionWithHaltCheck)._2.take(2).max

  var registers = Vector.fill(6)(0)
  var seen = Set[Int]()
  var last = 0

  while(registers(elfCode.ip) != instructionWithHaltCheck || !seen.contains(registers(registerWithNumberForExit))) {
    if (registers(elfCode.ip) == instructionWithHaltCheck) {
      last = registers(registerWithNumberForExit)
      if (seen.isEmpty) {
        printDayPart(1, last)
      }
      seen += last
    }
    registers = elfCode.runInstruction(registers)
  }

  printDayPart(2, last)
}

/*
0  seti 123 0 4         # r4 = 123
1  bani 4 456 4         # r4 = r4 & 456
2  eqri 4 72 4          # if (r4 == 72)
3  addr 4 1 1           #   GOTO 5
4  seti 0 0 1           # else GOTO 1
5  seti 0 8 4           # r4 = 0
6  bori 4 65536 3       # r3 = r4 | 65536
7  seti 16098955 8 4    # r4 = 16098955
8  bani 3 255 5         # r5 = r3 & 255
9  addr 4 5 4           # r4 += r5
10 bani 4 16777215 4    # r4 = r4 | 16777215
11 muli 4 65899 4       # r4 *= 65899
12 bani 4 16777215 4    # r4 = r4 | 16777215
13 gtir 256 3 5         # if (256 > r3)
14 addr 5 1 1           #   GOTO 16 --> GOTO 28 --> EXIT
15 addi 1 1 1           # else GOTO 17
16 seti 27 3 1          # GOTO 28
17 seti 0 7 5           # r5 = 0
18 addi 5 1 2           # r2 = r5 + 1
19 muli 2 256 2         # r2 *= 256
20 gtrr 2 3 2           # if (r2 > r3)
21 addr 2 1 1           #   GOTO 23 --> GOTO 26
22 addi 1 1 1           # else GOTO 24
23 seti 25 1 1          # GOTO 26
24 addi 5 1 5           # r5 += 1
25 seti 17 6 1          # GOTO 18
26 setr 5 4 3           # r3 = r5
27 seti 7 5 1           # GOTO 8
28 eqrr 4 0 5           # if (r4 == r0)
29 addr 5 1 1           #   EXIT
30 seti 5 3 1           # else GOTO 6
 */
