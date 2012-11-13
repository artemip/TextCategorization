import math.log
import math.abs

class Attribute(val word : String,
                val wordId: Int,
                private val atheismFoundResults: Int,
                private val graphicsFoundResults: Int) extends Ordered[Attribute] {

  private val total : Double = atheismFoundResults + graphicsFoundResults + 2

  val foundGivenAtheism = atheismFoundResults + 1 / total
  val foundGivenGraphics = graphicsFoundResults + 1 / total

  val discrimination = abs(log(foundGivenAtheism) - log(foundGivenGraphics))

  override def toString() = "(Word: " + word + ", Discrimination: " + discrimination + ")"
  def compare(other : Attribute) = other.discrimination.compareTo(discrimination)
}
