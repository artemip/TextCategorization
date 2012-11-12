import scala.math._

class Word(val value : String, private val found : List[Outcome], private val notFound : List[Outcome]) {
  private var informationGain : Option[Double] = None

  def getInformationGain() : Double = {
    if (informationGain.isDefined)
      return informationGain.get

    val (totalDistribution, foundDistribution, notFoundDistribution) = {
      val trueFoundResults = found.count(_ == true)
      val trueNotFoundResults= notFound.count(_ == true)
      val falseFoundResults = found.size - trueFoundResults
      val falseNotFoundResults = notFound.size - trueNotFoundResults

      (Distribution(trueFoundResults + trueNotFoundResults, falseFoundResults + falseNotFoundResults),
        Distribution(trueFoundResults, falseFoundResults),
        Distribution(trueNotFoundResults, falseNotFoundResults))
    }

    def entropy(dist : Distribution) = {
      val total = dist.positive + dist.negative

      val pos = dist.positive / total
      val neg = dist.negative / total

      (-pos * log(pos)) - (neg * log(neg))
    }

    val wordEntropy = entropy(totalDistribution)
    val foundEntropy = ((foundDistribution.positive + foundDistribution.negative) / (totalDistribution.positive + totalDistribution.negative)) * entropy(foundDistribution)
    val notFoundEntropy = ((notFoundDistribution.positive + notFoundDistribution.negative) / (totalDistribution.positive + totalDistribution.negative)) * entropy(notFoundDistribution)

    informationGain = Some(wordEntropy - foundEntropy - notFoundEntropy)

    informationGain.get
  }
}

case class Distribution(positive : Int, negative : Int)

case class Outcome(private val value : Boolean) {
  def apply() = value
}
