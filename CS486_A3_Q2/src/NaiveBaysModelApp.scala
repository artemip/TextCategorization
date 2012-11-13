import math.log

object NaiveBaysModelApp extends App {
  def getML(document : Document) = {
    var atheismHypothesis = 0.0
    var graphicsHypothesis = 0.0

    for (word <- TrainingData.wordAttributes) {
      if (document.containsWord(word.wordId)) {
        atheismHypothesis = atheismHypothesis + log(word.foundGivenAtheism)
        graphicsHypothesis = graphicsHypothesis + log(word.foundGivenGraphics)
      }
    }

    if (atheismHypothesis > graphicsHypothesis)
      1
    else
      2
  }

  var numCorrect = 0
  val numTotalTrainingDocs = TrainingData.trainDocuments.size

  for (doc <- TrainingData.trainDocuments) {
    val classification = getML(doc)
    if (classification == doc.label) numCorrect = numCorrect + 1
  }

  println("% Correct Training = " + (numCorrect / numTotalTrainingDocs.toDouble))

  numCorrect = 0
  val testingDocs = TrainingData.extractDocuments(false)
  val numTotalTestingDocs = testingDocs.size

  for (doc <- testingDocs) {
    val classification = getML(doc)
    if (classification == doc.label) numCorrect = numCorrect + 1
  }

  println("% Correct Testing = " + (numCorrect / numTotalTestingDocs.toDouble))

  println("--- Most discriminative word features ---")
  TrainingData.wordAttributes.sorted.take(10).foreach(println(_))

}
