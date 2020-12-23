package adventofcode.common.collections

import scala.collection.mutable

class DoubleLinkedListWithIndex[A](firstValue: A) {
  private val index = mutable.Map.empty[A, Node[A]]
  index.update(firstValue, new Node(firstValue))

  def get(value: A): Node[A] = index(value)
  def contains(value: A): Boolean = index.contains(value)
  def size: Int = index.size

  def insertAfter(value: A, prev: Node[A]): Node[A] = {
    val node = prev.insert(value)
    index.update(value, node)
    node
  }

  def remove(value: A): Unit = {
    index.remove(value).get.remove()
  }
}

class Node[V](var value: V) {
  private var prevNode: Node[V] = this
  private var nextNode: Node[V] = this

  def prev: Node[V] = prevNode
  def next: Node[V] = nextNode

  private[collections] def insert(value: V): Node[V] = {
    val node = new Node(value)
    node.prevNode = this
    node.nextNode = this.nextNode
    nextNode.prevNode = node
    nextNode = node
    node
  }

  private[collections] def remove(): Unit = {
    nextNode.prevNode = prevNode
    prevNode.nextNode = nextNode
  }
}
