package com.josephus

/*
*   Using circular list datastructure to solve problem.
*   Problem can always be solved provided starting position < noPeople
*   If stepRate >= noPeople , positions will be counted in joined entities, problem will still be solved
* */

class josephus(val noPeople: Int, val stepRate: Int, val startingPosition: Int) {
  val peopleAtStart = {
    val doubleLinkList = new DoubleLinkList[Int]
    for (person <- noPeople to 1 by -1) {
      doubleLinkList.addFirst(person)
    }
    doubleLinkList
  }

  def findSurvivor(): DoubleLinkList[Int] = {

    println(s"Beginning Josephus Elimination: Number of people:${noPeople}, Step Rate: ${stepRate}, Starting Position: ${startingPosition}")

    iterateAndRemovePeople

    println("Survivor is: " + peopleAtStart.values(0))
    peopleAtStart
  }

  private def iterateAndRemovePeople: Unit = {
    var currentNode = getStartingNode
    var currentIndex = 1
    while (peopleAtStart.size > 1) {

      if (currentIndex % stepRate == 0) {
        println("removing " + currentNode)
        peopleAtStart.remove(currentNode)
      }
      currentIndex += 1
      currentNode.next match {
        case SentinelNode() => currentNode = currentNode.next.next
        case Node(_) => currentNode = currentNode.next
      }

    }
  }

  def getStartingNode: ListNode[Int] = {
    var currentNode = peopleAtStart.first
    if (startingPosition > 1) {
      for (position <- 1 to startingPosition) {
        currentNode = currentNode.next
      }
    }
    currentNode
  }
}

object executor extends App {
  new josephus(10,2,1).findSurvivor()
}