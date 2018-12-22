package adventofcode.y2018

import adventofcode.common.Pos

object Day13 extends Year2018 {
  override val day = 13

  // down, right, up, left
  private val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

  private val railsAndCarts = input.getLines().zipWithIndex.map {
    case (line, y) =>
      line.zipWithIndex.map {
        case ('v', x) => ('|', Some(Cart(Pos(x, y), directions(0))))
        case ('>', x) => ('-', Some(Cart(Pos(x, y), directions(1))))
        case ('^', x) => ('|', Some(Cart(Pos(x, y), directions(2))))
        case ('<', x) => ('-', Some(Cart(Pos(x, y), directions(3))))
        case (rail, _) => (rail, None)
      }
  }.toList

  private val (map, carts) = (railsAndCarts.map(_.map(_._1)), railsAndCarts.flatMap(_.flatMap(_._2)))
  private val (crashes, lastCart) = simulate(carts)

  printDayPart(1, crashes.head.toString, "first crash at: %s")
  printDayPart(2, lastCart.toString, "last cart at: %s")

  private def simulate(carts: List[Cart], cartsDone: List[Cart] = Nil, crashes: List[Pos] = Nil): (List[Pos], Pos) = {
    carts match {
      case Nil if cartsDone.size <= 1 => (crashes.reverse, cartsDone.head.pos)
      case Nil => simulate(cartsDone.sortBy(c => (c.pos.y, c.pos.x)), Nil, crashes)
      case currentCart :: cartsTodo =>
        val movedCart = currentCart.forward
        if ((cartsTodo ::: cartsDone).exists(_.pos == movedCart.pos)) {
          simulate(cartsTodo.filterNot(_.pos == movedCart.pos), cartsDone.filterNot(_.pos == movedCart.pos), movedCart.pos :: crashes)
        } else {
          val turnedCart = (getMapAt(movedCart.pos), movedCart.direction) match {
            case ('+', _) => movedCart.nextTurn match {
              case -1 => movedCart.turnLeft.copy(nextTurn = 0)
              case  0 => movedCart.copy(nextTurn = 1)
              case  1 => movedCart.turnRight.copy(nextTurn = -1)
            }
            case ('/', (0, _))|('\\', (_, 0)) => movedCart.turnRight
            case ('/', (_, 0))|('\\', (0, _)) => movedCart.turnLeft
            case _ => movedCart
          }
          simulate(cartsTodo, turnedCart :: cartsDone, crashes)
        }
    }
  }

  private def getMapAt(pos: Pos) = map(pos.y)(pos.x)

  private case class Cart(pos: Pos, direction: (Int, Int), nextTurn: Int = -1) {
    def forward: Cart = copy(pos = pos + direction)
    def turnLeft: Cart = copy(direction = directions((directions.indexOf(direction) + 1) % 4))
    def turnRight: Cart = copy(direction = directions((directions.indexOf(direction) + 3) % 4))
  }
}
