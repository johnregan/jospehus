package com.josephus

trait ListNode[T] {
  var next: ListNode[T]
  var prev: ListNode[T]
  def isEmpty: Boolean
}

case class SentinelNode[T]() extends ListNode[T] {
  var next: ListNode[T] = this
  var prev: ListNode[T] = this
  def isEmpty = true

  override def toString(): String = {
    "*"
  }
}

case class Node[T](val element: T) extends ListNode[T] {
  var prev: ListNode[T] = null
  var next: ListNode[T] = null
  def isEmpty = false

  override def toString: String = {
    " " + element + " "
  }
}

class DoubleLinkList[T]() {
  private var sentinel: ListNode[T] = new SentinelNode[T]
  var first: ListNode[T] = sentinel
  var last: ListNode[T] = sentinel
  var size: Int = 0

  def values: List[T] = {
    def accumValues(accum: List[T], previousNode: ListNode[T]): List[T] = {
      previousNode match {
        case _ if accum.size == size => accum
        case SentinelNode()  => accumValues(accum, previousNode.next)
        case Node(element) => accumValues(List(element) ::: accum, previousNode.next)
      }
    }
    accumValues(List(), first)
  }

  def addFirst(element: T): ListNode[T] = {
    val node=addNewNode(element)
    first.prev = node
    first = node
    node
  }

  def addLast(element: T): ListNode[T] = {
    val node=addNewNode(element)
    last.next = node
    last = node
    node
  }

  private def addNewNode(element: T): ListNode[T] = {
    val node = new Node[T](element)
    node.prev = last
    node.next = first

    if (sentinel == first) {
      sentinel.prev = node
      node.next = sentinel
    }

    if (sentinel == last) {
      sentinel.next = node
      node.prev = sentinel
    }
    size = size + 1

    node
  }

  def remove(node: ListNode[T]): Unit = {
    var previousNode = node.prev
    var nextNode = node.next

    nextNode.prev = previousNode
    previousNode.next = nextNode

    if (node == last) {
      last = previousNode
    }

    if (node == first) {
      first = nextNode
    }
    size = size - 1
  }

  override def toString: String = {
    var currentNode = first
    var str = "DoubleLinkList["
    for (i <- 0 to size) {
      str += currentNode + ""
      currentNode = currentNode.next
    }
    str += "]"
    str
  }
}