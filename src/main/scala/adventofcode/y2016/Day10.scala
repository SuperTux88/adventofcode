package adventofcode.y2016

import scala.io.BufferedSource

object Day10 extends Year2016 {
  override val day: Int = 10

  private val ValueRE = """value (\d+) goes to bot (\d+)""".r
  private val InstructionRE = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    var bots = Map.empty[Int, List[Int]]
    var botInstructions = Map.empty[Int, List[Int] => Unit]

    var outputs = Map.empty[Int, Int]

    def addChipToBot(id: Int, chip: Int): Unit = {
      bots = bots + (id -> (chip :: bots.getOrElse(id, List.empty[Int])).sorted)
    }

    def addChipToTarget(targetType: String, id: Int, value: Int): Unit = {
      targetType match {
        case "bot" => addChipToBot(id, value)
        case "output" => outputs = outputs + (id -> value)
      }
    }

    input.getLines().foreach {
      case ValueRE(value, bot) => addChipToBot(bot.toInt, value.toInt)
      case InstructionRE(bot, lowType, lowId, highType, highId) =>
        botInstructions = botInstructions + (bot.toInt -> { chips =>
          addChipToTarget(lowType, lowId.toInt, chips.head)
          addChipToTarget(highType, highId.toInt, chips.last)
        })
    }

    while (bots.nonEmpty) {
      bots.filter(_._2.size == 2).foreach { bot =>
        val (botId, chips) = bot

        if (chips == List(17, 61)) printDayPart(1, botId)

        botInstructions(botId)(chips)
        bots -= botId
      }
    }

    printDayPart(2, outputs(0) * outputs(1) * outputs(2))
  }
}
