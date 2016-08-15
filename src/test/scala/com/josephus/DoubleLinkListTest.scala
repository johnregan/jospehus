package com.josephus

import org.scalatest.{FlatSpec, Matchers}

class DoubleLinkListTest extends FlatSpec with Matchers {

  "A double linked list" should "return sentinel when empty" in {
    val doubleLinkList = new DoubleLinkList[Int]

    doubleLinkList.first shouldEqual doubleLinkList.last
    nodeMatchesSentinel(doubleLinkList.last.prev.prev) shouldEqual true
  }

  it should "add an element first correctly" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addFirst(5)

    nodeValue(doubleLinkList.first) shouldEqual 5
    nodeMatchesSentinel(doubleLinkList.first.next) shouldEqual true
    doubleLinkList should have size (1)
  }

  it should "add multiple elements first correctly" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addFirst(5)
    doubleLinkList.addFirst(4)

    nodeValue(doubleLinkList.first) shouldEqual 4
    nodeValue(doubleLinkList.first.next) shouldEqual 5
    nodeMatchesSentinel(doubleLinkList.first.next.next) shouldEqual true
    doubleLinkList should have size (2)
  }

  it should "add an element last correctly" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addLast(5)

    nodeValue(doubleLinkList.last) shouldEqual 5
    nodeMatchesSentinel(doubleLinkList.last.next) shouldEqual true
    doubleLinkList should have size (1)
  }

  it should "add multiple elements last correctly" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addLast(5)
    doubleLinkList.addLast(4)

    nodeValue(doubleLinkList.last) shouldEqual 4
    nodeValue(doubleLinkList.last.prev) shouldEqual 5
    nodeMatchesSentinel(doubleLinkList.last.next) shouldEqual true
    doubleLinkList should have size (2)
  }

  it should "remove nodes correctly when nodes added first" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addFirst(5)
    doubleLinkList.addFirst(4)
    val removeNode = doubleLinkList.addFirst(3)
    doubleLinkList.addFirst(2)
    doubleLinkList.addFirst(1)


    doubleLinkList.remove(removeNode)
    //1,2,4,5,*
    nodeValue(doubleLinkList.first) shouldEqual 1
    nodeValue(doubleLinkList.first.next) shouldEqual 2
    nodeValue(doubleLinkList.first.next.next) shouldEqual 4
    nodeValue(doubleLinkList.first) shouldEqual 1
    nodeMatchesSentinel(doubleLinkList.last) shouldEqual true
    nodeValue(doubleLinkList.last.prev) shouldEqual 5

    doubleLinkList should have size (4)
  }

  it should "remove nodes correctly when nodes added last" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addLast(5)
    doubleLinkList.addLast(4)
    val removeNode = doubleLinkList.addLast(3)
    doubleLinkList.addLast(2)
    doubleLinkList.addLast(1)


    doubleLinkList.remove(removeNode)
    //*,5,4,2,1

    nodeMatchesSentinel(doubleLinkList.first) shouldEqual true
    nodeValue(doubleLinkList.last) shouldEqual 1
    nodeValue(doubleLinkList.last.prev) shouldEqual 2
    nodeValue(doubleLinkList.last.prev.prev) shouldEqual 4
    nodeValue(doubleLinkList.last.prev.prev.prev) shouldEqual 5

    doubleLinkList should have size (4)
  }

  it should "remove nodes correctly when nodes added to both sides" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addFirst(5)
    doubleLinkList.addFirst(4)
    val removeNode = doubleLinkList.addLast(3)
    doubleLinkList.addLast(2)
    doubleLinkList.addLast(1)
    doubleLinkList.remove(removeNode)

    //45*21
    nodeValue(doubleLinkList.first) shouldEqual 4
    nodeValue(doubleLinkList.first.next) shouldEqual 5
    nodeMatchesSentinel(doubleLinkList.first.next.next) shouldEqual true
    nodeValue(doubleLinkList.last) shouldEqual 1
    nodeValue(doubleLinkList.last.prev) shouldEqual 2
    nodeMatchesSentinel(doubleLinkList.last.prev.prev) shouldEqual true
    nodeValue(doubleLinkList.last.next) shouldEqual 4

    doubleLinkList should have size (4)
  }

  it should "show all values in list" in {
    val doubleLinkList = new DoubleLinkList[Int]
    doubleLinkList.addFirst(5)
    doubleLinkList.addFirst(4)
    val removeNode = doubleLinkList.addLast(3)
    doubleLinkList.addLast(2)
    doubleLinkList.addLast(1)
    doubleLinkList.addFirst(6)

    doubleLinkList.remove(removeNode)

    doubleLinkList.values should contain allOf(6, 4, 5, 2, 1)
  }

  def nodeValue(node: ListNode[Int]): Int = {
    node match {
      case Node(element) => element
    }
  }

  def nodeMatchesSentinel(node: ListNode[Int]): Boolean = {
    node match {
      case SentinelNode() => true
    }
  }
}