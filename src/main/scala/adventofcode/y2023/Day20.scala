package adventofcode.y2023

import adventofcode.common.NumberHelper

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.BufferedSource

object Day20 extends Year2023 {
  override val day = 20

  private val ModuleRE = """([%&]?)(\w+) -> (.+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val (parsedModules, outputMappings) = input.getLines().takeWhile(_.nonEmpty)
      .foldLeft(Map.empty[String, Module], Map.empty[String, List[String]]) { case ((modules, mappings), line) =>
        line match {
          case ModuleRE("", "broadcaster", outputs) =>
            (modules + ("broadcaster" -> Broadcaster()), mappings + ("broadcaster" -> splitOutputs(outputs)))
          case ModuleRE("%", name, outputs) =>
            (modules + (name -> FlipFlop(name)), mappings + (name -> splitOutputs(outputs)))
          case ModuleRE("&", name, outputs) =>
            (modules + (name -> Conjunction(name)), mappings + (name -> splitOutputs(outputs)))
        }
      }
    val modules = parsedModules.view.mapValues {
      case c: Conjunction => c.copy(state = outputMappings.filter(_._2.contains(c.name)).keys.map(k => k -> false).toMap)
      case m => m
    }.toMap.withDefaultValue(Output())

    val (_, (low, high)) = (1 to 1000).foldLeft((modules, (0L, 0L))) { case ((state, (low, high)), _) =>
      val (newState, (newLow, newHigh)) = simulate(outputMappings, state)
      (newState, (low + newLow, high + newHigh))
    }
    printDayPart(1, low * high, "Products of low and high pulses sent: %s")

    val rxInput = outputMappings.find(_._2.contains("rx")).get._1
    val conjunctionInputs = outputMappings.filter(_._2.contains(rxInput)).keys.toList
    val loops = conjunctionInputs.map(findConjunctionLoop(outputMappings, modules, rxInput, _))

    printDayPart(2, NumberHelper.lcm(loops), "The rx module receives a low pulse after %s button presses")
  }

  private def simulate(outputs: Map[String, List[String]], state: Map[String, Module]) = {
    @tailrec
    def loop(state: Map[String, Module], queue: Queue[(String, String, Boolean)],
             count: (Long, Long) = (0, 0)): (Map[String, Module], (Long, Long)) = {
      if (queue.isEmpty) (state, count)
      else {
        val ((from, name, signal), dequeue) = queue.dequeue
        // println(s"$from -> $signal -> $name")
        val newModule = state(name).receive(from, signal)
        val newState = state.updated(name, newModule)
        val newCount = if (signal) (count._1, count._2 + 1) else (count._1 + 1, count._2)
        newModule.output match {
          case None =>
            loop(newState, dequeue, newCount)
          case Some(newSignal) =>
            loop(newState, outputs(name).foldLeft(dequeue)((q, out) => q.enqueue(name, out, newSignal)), newCount)
        }
      }
    }

    loop(state, Queue(("button", "broadcaster", false)))
  }

  private def findConjunctionLoop(outputs: Map[String, List[String]], state: Map[String, Module],
                                  conjunction: String, module: String): Long = {
    @tailrec
    def loop(state: Map[String, Module], loops: Long = 0): Long = {
      if (state(conjunction).asInstanceOf[Conjunction].wasTrue.contains(module))
        loops
      else
        loop(simulate(outputs, state)._1, loops + 1)
    }

    loop(state)
  }

  private def splitOutputs(outputs: String): List[String] = outputs.split(", ").toList

  private sealed trait Module {
    val name: String
    def receive(from: String, received: Boolean): Module
    def output: Option[Boolean]
  }
  private case class Broadcaster() extends Module {
    override val name: String = "broadcaster"
    override def receive(from: String, received: Boolean): Module = this
    override def output: Option[Boolean] = Some(false)
  }
  private case class Output() extends Module {
    override val name: String = "output"
    override def receive(from: String, received: Boolean): Module = this
    override def output: Option[Boolean] = None
  }
  private case class FlipFlop(name: String, last: Boolean = false, state: Boolean = false) extends Module {
    override def receive(from: String, received: Boolean): Module =
      if (received) copy(last = received) else copy(last = received, state = !state)
    override def output: Option[Boolean] = if (last) None else Some(state)
  }
  private case class Conjunction(name: String, state: Map[String, Boolean] = Map.empty,
                                 wasTrue: Set[String] = Set.empty) extends Module {
    override def receive(from: String, received: Boolean): Module = {
      val newState = state.updated(from, received)
      if (received) copy(state = newState, wasTrue = wasTrue + from)
      else copy(state = newState)
    }
    override def output: Option[Boolean] = Some(!state.values.forall(identity))
  }
}
