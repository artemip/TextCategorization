import collection.mutable.PriorityQueue

object DecisionTree {
  def apply(numNodes : Int, decisionNodeQueue : PriorityQueue[DecisionNode]) : DecisionNode = {

    var nodeQueue = decisionNodeQueue
    val leafQueue = new PriorityQueue[Leaf]()

    def getNextDecisionNode() = {
      val node = nodeQueue.head.clone()
      nodeQueue = nodeQueue.tail

      node
    }

    val root = getNextDecisionNode()

    for(leaf <- root.getLeaves) {
      leafQueue.enqueue(leaf)
    }

    for(i <- 1 until numNodes) {
      val highestIGLeaf = leafQueue.dequeue()
      val newDecisionNode = getNextDecisionNode()

      highestIGLeaf.expand(newDecisionNode)

      for(leaf <- newDecisionNode.getLeaves())
        leafQueue.enqueue(leaf)

    }

    root
  }
}