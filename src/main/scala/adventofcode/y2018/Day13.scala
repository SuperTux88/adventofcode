package adventofcode.y2018

import adventofcode.common.pos.Direction.DirectionPos
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day13 extends Year2018 {
  override val day = 13

  override def runDay(input: BufferedSource): Unit = {
    val railsAndCarts = input.getLines().zipWithIndex.map {
      case (line, y) =>
        line.zipWithIndex.map {
          case ('v', x) => ('|', Some(Cart(Pos(x, y), Direction.down)))
          case ('>', x) => ('-', Some(Cart(Pos(x, y), Direction.right)))
          case ('^', x) => ('|', Some(Cart(Pos(x, y), Direction.up)))
          case ('<', x) => ('-', Some(Cart(Pos(x, y), Direction.left)))
          case (rail, _) => (rail, None)
        }
    }.toList

    val (map, carts) = (railsAndCarts.map(_.map(_._1)), railsAndCarts.flatMap(_.flatMap(_._2)))
    val (crashes, lastCart) = simulate(map, carts)

    printDayPart(1, crashes.head.toString, "first crash at: %s")
    printDayPart(2, lastCart.toString, "last cart at: %s")
  }

  @tailrec
  private def simulate(map: List[IndexedSeq[Char]], carts: List[Cart], cartsDone: List[Cart] = Nil, crashes: List[Pos] = Nil): (List[Pos], Pos) = {
    carts match {
      case Nil if cartsDone.size <= 1 => (crashes.reverse, cartsDone.head.pos)
      case Nil => simulate(map, cartsDone.sortBy(c => (c.pos.y, c.pos.x)), Nil, crashes)
      case currentCart :: cartsTodo =>
        val movedCart = currentCart.forward
        if ((cartsTodo ::: cartsDone).exists(_.pos == movedCart.pos)) {
          simulate(map, cartsTodo.filterNot(_.pos == movedCart.pos), cartsDone.filterNot(_.pos == movedCart.pos), movedCart.pos :: crashes)
        } else {
          val turnedCart = (getMapAt(map, movedCart.pos), movedCart.direction) match {
            case ('+', _) => movedCart.nextTurn match {
              case -1 => movedCart.turnLeft.copy(nextTurn = 0)
              case  0 => movedCart.copy(nextTurn = 1)
              case  1 => movedCart.turnRight.copy(nextTurn = -1)
            }
            case ('/', Pos(0, _))|('\\', Pos(_, 0)) => movedCart.turnRight
            case ('/', Pos(_, 0))|('\\', Pos(0, _)) => movedCart.turnLeft
            case _ => movedCart
          }
          simulate(map, cartsTodo, turnedCart :: cartsDone, crashes)
        }
    }
  }

  private def getMapAt(map: List[IndexedSeq[Char]], pos: Pos) = map(pos.y)(pos.x)

  private case class Cart(pos: Pos, direction: Pos, nextTurn: Int = -1) {
    def forward: Cart = copy(pos = pos + direction)
    def turnLeft: Cart = copy(direction = direction.rotateLeft)
    def turnRight: Cart = copy(direction = direction.rotateRight)
  }
}
