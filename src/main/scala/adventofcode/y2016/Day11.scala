package adventofcode.y2016

import adventofcode.Logging

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day11 extends Year2016 {
  override val day: Int = 11

  private val FloorRE = """The (\w+) floor contains ([\w ,-]+)\.""".r
  private val GeneratorRE = """a (\w+) generator""".r
  private val MicrochipRE = """a (\w+)-compatible microchip""".r

  override def runDay(input: BufferedSource): Unit = {
    val floors = input.getLines().map {
      case FloorRE(floor, content) => getFloorNumber(floor) -> (content match {
        case "nothing relevant" => Floor(List.empty)
        case components => Floor(components.split("(,? and|,) ").map {
          case GeneratorRE(element) => Generator(element)
          case MicrochipRE(element) => Microchip(element)
        }.toList)
      })
    }.toMap

    val startPart1 = State(floors, 1, List.empty)
    val additionalPart2 = List(Generator("elerium"), Microchip("elerium"), Generator("dilithium"), Microchip("dilithium"))
    val startPart2 = State(floors + (1 -> Floor(floors(1).components ::: additionalPart2)), 1, List.empty)

    var knownStates = List(startPart1.getStateFingerprint)

    val solution = getSolution(List(startPart1), knownStates).get
    printDayPart(1, solution.previousStates.length)

    knownStates = List(startPart2.getStateFingerprint)

    val solution2 = getSolution(List(startPart2), knownStates).get
    printDayPart(2, solution2.previousStates.length)
  }

  @tailrec
  private def getSolution(states: List[State], knownStates: List[(Int, List[List[Int]])]): Option[State] = {
    if (Logging.debug) println(s"${states.head.previousStates.size} | ${states.size} | ${knownStates.size}")

    val solution = states.find(_.isDone)
    if (solution.isDefined) {
      solution
    } else {
      val (nextStates, nextKnownStates) = states.foldLeft((List.empty[State], knownStates)) { (currentStates, state) =>
        val (nextState, nextKnownStates) = state.nextStates(currentStates._2)
        (currentStates._1 ::: nextState, nextKnownStates)
      }
      if (nextStates.isEmpty)
        None
      else
        getSolution(nextStates, nextKnownStates)
    }
  }

  private trait Component {
    def element: String
    def compatible(component: Component): Boolean = component.element == element
  }
  private case class Generator(element: String) extends Component
  private case class Microchip(element: String) extends Component

  private case class Floor(components: List[Component]) {
    lazy val generators: List[Generator] = components.collect { case g: Generator => g }
    lazy val microchips: List[Microchip] = components.collect { case m: Microchip => m }

    def isEmpty: Boolean = components.isEmpty
    def isOK: Boolean = generators.isEmpty || unprotectedChips.isEmpty

    def combinations: List[List[Component]] = (1 to 2).flatMap(x => components.combinations(x)).toList

    def remove(comps: List[Component]): Floor = Floor(components.filterNot(comps.contains))
    def add(comps: List[Component]): Floor = Floor(components ::: comps)

    private def unprotectedChips = microchips.filterNot(m => generators.exists(_.compatible(m)))
  }

  private case class State(floors: Map[Int, Floor], position: Int, previousStates: List[State]) {
    def isDone: Boolean = position == 4 && lowerFloorsEmpty

    def getStateFingerprint: (Int, List[List[Int]]) = {
      val pairs = floors.flatMap { floor =>
        floor._2.components.map((_, floor._1))
      }.groupBy(_._1.element).map(_._2.values.toList.sorted).toList.sortBy(_.head)
      (position, pairs)
    }

    def nextStates(knownStates: List[(Int, List[List[Int]])]): (List[State], List[(Int, List[List[Int]])]) = {
      val combinations = floors(position).combinations
      val nextStates = nextStatesForFloor(combinations, position + 1) ::: nextStatesForFloor(combinations, position - 1)

      val newStates = nextStates.filterNot(state => knownStates.contains(state.getStateFingerprint))
      val uniqueStates = newStates.groupBy(_.getStateFingerprint).values.map(_.head).toList
      (uniqueStates, knownStates ::: uniqueStates.map(_.getStateFingerprint))
    }

    private def nextStatesForFloor(combinations: List[List[Component]], to: Int): List[State] = {
      if (!floors.contains(to) || (to < position && lowerFloorsEmpty)) {
        List.empty
      } else {
        val nextStates = combinations.map { components =>
          val newFloors = floors + (position -> floors(position).remove(components)) +
                                   (to -> floors(to).add(components))
          (components.length, State(newFloors, to, this :: previousStates))
        }.filter(_._2.isOK)

        filterOptimalStates(nextStates, to).map(_._2)
      }
    }

    private def filterOptimalStates(states: List[(Int, State)], to: Int): List[(Int, State)] = {
      if (to < position && states.exists(_._1 == 1))
        states.filter(_._1 == 1)
      else if (to > position && states.exists(_._1 == 2))
        states.filter(_._1 == 2)
      else
        states
    }

    private def isOK = floors.values.toList.forall(_.isOK)
    private def lowerFloorsEmpty = position > 1 && (1 until position).forall(floors(_).isEmpty)
  }

  private def getFloorNumber(string: String) = string match {
    case "first"  => 1
    case "second" => 2
    case "third"  => 3
    case "fourth" => 4
  }
}
