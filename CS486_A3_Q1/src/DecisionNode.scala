import math._

trait Node {
  def getInformationGain() : Double
  def classify(document : Document) : String
  def print() : Unit = print("", "")
  def print(prefix : String, postFix : String) : Unit
}

case class DecisionNode(val value: String,
                   val wordId: Int,
                   private val atheismFoundResults: Int,
                   private val graphicsFoundResults: Int,
                   private val atheismNotFoundResults: Int,
                   private val graphicsNotFoundResults: Int) extends Node with Ordered[DecisionNode] with Cloneable {

  private val (totalDistribution, foundDistribution, notFoundDistribution) = {
    (Distribution(atheismFoundResults + atheismNotFoundResults, graphicsFoundResults + graphicsNotFoundResults),
      Distribution(atheismFoundResults, graphicsFoundResults),
      Distribution(atheismNotFoundResults, graphicsNotFoundResults))
  }
  private val (nodeEntropy, foundEntropy, notFoundEntropy) = {
    (entropy(totalDistribution),
      entropy(foundDistribution),// * (foundDistribution.total).toDouble / (totalDistribution.total),
      entropy(notFoundDistribution))// * (notFoundDistribution.total).toDouble / (totalDistribution.total))
  }

  private val informationGain = nodeEntropy - ((foundDistribution.total).toDouble / (totalDistribution.total) * foundEntropy + (notFoundDistribution.total).toDouble / (totalDistribution.total) * notFoundEntropy)

  private var foundChild : Node = new Leaf(this, foundEntropy, foundDistribution)
  private var notFoundChild : Node = new Leaf(this, notFoundEntropy, notFoundDistribution)

  def getInformationGain() = informationGain

  def expandLeaf(decisionNode : DecisionNode) {
    (foundChild, notFoundChild) match {
      case (f : Leaf, nf : DecisionNode) => foundChild = decisionNode
      case (f : DecisionNode, nf : Leaf) => notFoundChild = decisionNode
      case (f : Leaf, nf : Leaf) => {
        if(foundChild.getInformationGain() > notFoundChild.getInformationGain()) {
          foundChild = decisionNode
        } else {
          notFoundChild = decisionNode
        }
      }
    }
  }

  def getLeaves() : List[Leaf] = {
    (foundChild, notFoundChild) match {
      case (f : Leaf, nf : DecisionNode) => List(f)
      case (f : DecisionNode, nf : Leaf) => List(nf)
      case (f : Leaf, nf : Leaf) => List(f, nf)
      case _ => {
        List()
      }
    }
  }

  private def entropy(dist: Distribution): Double = {
    val pos = dist.atheism / dist.total.toDouble
    val neg = dist.graphics / dist.total.toDouble

    if (dist.total == 0 || pos == 0 || neg == 0) return 0.0

    (-pos * log(pos)) - (neg * log(neg))
  }

  def compare(other: DecisionNode) = getInformationGain.compare(other.getInformationGain)
  override def toString() = "(Value: " + value + ", IG: " + informationGain + ")"
  override def clone() = new DecisionNode(value, wordId, atheismFoundResults, graphicsFoundResults, atheismNotFoundResults, graphicsNotFoundResults)

  //Classification-related operations
  def classify(document : Document) : String = {
    if (document.containsWord(wordId)) { //Take Found branch
      foundChild.classify(document)
    } else { //Take NotFound branch
      notFoundChild.classify(document)
    }
  }
  def print(prefix : String, postFix : String = "") {
    val name = value + ", IG: " + informationGain
    val thisPrefix = prefix + "├── " + postFix
    val childPrefix = prefix + "│   "
    println(thisPrefix + name)

    (foundChild, notFoundChild) match {
      case (_ : Leaf, _) => {
        notFoundChild.print(childPrefix, "(NotFound) ")
        foundChild.print(childPrefix,    "(Found) ")
      } case (_, _ :Leaf) => {
        foundChild.print(childPrefix,    "(Found) ")
        notFoundChild.print(childPrefix, "(NotFound) ")
      } case _ => {
        foundChild.print(childPrefix,    "(Found) ")
        notFoundChild.print(childPrefix, "(NotFound) ")
      }
    }
  }
}

case class Leaf(private val parent : DecisionNode, private val informationGain : Double, private val distribution : Distribution) extends Node with Ordered[Leaf]{
  private val result = if(distribution.atheism > distribution.graphics) "alt.atheism" else "comp.graphics"

  def expand(decisionNode : DecisionNode) {
    parent.expandLeaf(decisionNode)
  }

  def getInformationGain() = {
    if (DecisionTreeApp.extendedInformationGain) {
      informationGain * distribution.total
    } else {
      informationGain
    }
  }

  def compare(other: Leaf) = getInformationGain.compare(other.getInformationGain)

  def classify(document : Document) = {
    result
  }

  def print(prefix : String, postFix : String) {
    println(prefix + "└── " + postFix + result + ", IG: " + getInformationGain())
  }
}

case class Distribution(atheism: Int, graphics: Int) {
  val total = atheism + graphics
}