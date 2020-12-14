package adventofcode.y2020

object Day14 extends Year2020 {
  override val day = 14

  private val MaskRE = """mask = (\w+)""".r
  private val MemoryRE = """mem\[(\d+)] = (\d+)""".r

  private val program = input.getLines().map {
    case MaskRE(mask) => Mask(mask)
    case MemoryRE(address, value) => Memory(address.toInt, value.toLong)
  }.toList

  printDayPart(1, runProgram(program, maskValue).values.sum, "sum of values in memory: %s")
  printDayPart(2, runProgram(program, maskToFloatingAddresses).values.sum, "sum of values in memory with floating addresses: %s")

  private def runProgram(instructions: List[Instruction], processMemoryInstruction: (Memory, State) => Map[Long, Long]) =
    instructions.tail.foldLeft(State(Map.empty[Long, Long], instructions.head.asInstanceOf[Mask])) { (state, instruction) =>
      instruction match {
        case newMask: Mask => state.copy(mask = newMask)
        case memoryInstruction: Memory =>
          state.copy(memory = processMemoryInstruction(memoryInstruction, state))
      }
    }.memory

  private def maskValue(memInstruction: Memory, state: State) =
    state.memory.updated(memInstruction.address,
      java.lang.Long.parseLong(applyMask(memInstruction.value, state.mask, 'X'), 2))

  private def maskToFloatingAddresses(memInstruction: Memory, state: State) =
    applyFloatings(applyMask(memInstruction.address, state.mask, '0'))
      .foldLeft(state.memory) { (memory, address) =>
        memory.updated(java.lang.Long.parseLong(address, 2), memInstruction.value)
      }

  private def applyMask(value: Long, mask: Mask, keepFromValue: Char) =
    value.toBinaryString.reverse.padTo(mask.mask.length, '0').reverse.zip(mask.mask).map { position =>
      position._2 match {
        case `keepFromValue` => position._1
        case fromMask => fromMask
      }
    }.mkString

  private def applyFloatings(address: String) =
    (0 until address.length).filter(address.charAt(_) == 'X').foldLeft(List(address)) { (list, index) =>
      list.map(_.updated(index, '0')) ::: list.map(_.updated(index, '1'))
    }

  private sealed trait Instruction
  private case class Mask(mask: String) extends Instruction
  private case class Memory(address: Int, value: Long) extends Instruction

  private case class State(memory: Map[Long, Long], mask: Mask)
}
