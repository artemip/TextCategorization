import collection.mutable
import collection.mutable.ListBuffer
import collection.mutable.Set
import io.Source

object TrainingData {

  def extractDocuments(training : Boolean) = {
    val (documentPath, labelPath) = if(training) ("data/trainData.txt", "data/trainLabel.txt") else ("data/testData.txt", "data/testLabel.txt")
    val documentLines = Source.fromFile(documentPath).getLines.toList
    val labelLines = Source.fromFile(labelPath).getLines.toArray.map(_.toInt)

    //Get first document
    val firstDoc = documentLines.head
    val (firstDocId, firstDocWordId) = {
      val splitLine = firstDoc.split("\t")
      if (splitLine.size > 0) (splitLine.head.toInt, splitLine.last.toInt) else (-1, -1)
    }
    var currentDoc : Document = new Document(labelLines(0))
    var currentDocId = firstDocId
    currentDoc.addWord(firstDocWordId)

    val documents = ListBuffer[Document]()

    for(docLine <- documentLines.tail) {
      val (docId, wordId) = {
        val splitLine = docLine.split("\t")
        if (splitLine.size > 0) (splitLine.head.toInt, splitLine.last.toInt) else (-1, -1)
      }

      //New document
      if(docId.compare(currentDocId) != 0) {
        documents prepend currentDoc
        currentDoc = new Document(labelLines(docId - 1))
        currentDocId = docId
      }

      currentDoc.addWord(wordId)
    }

    documents
  }

  lazy val (sortedNodesQueue : mutable.PriorityQueue[DecisionNode], trainDocuments : ListBuffer[Document]) = {
    val documents = extractDocuments(true)
    val wordLines = Source.fromFile("data/words.txt").getLines.toList
    val nodes = mutable.PriorityQueue[DecisionNode]()

    var index = 0

    for(wordLine <- wordLines) {
      index = index + 1

      var foundAtheism, foundGraphics, notFoundAtheism, notFoundGraphics = 0

      for(doc <- documents) {
        if (doc.containsWord(index)) { //Word found
          if(doc.label == 1) foundAtheism = foundAtheism + 1
          else foundGraphics = foundGraphics + 1
        } else {
          if(doc.label == 1) notFoundAtheism = notFoundAtheism + 1
          else notFoundGraphics = notFoundGraphics + 1
        }
      }

      nodes enqueue new DecisionNode(wordLine, index, foundAtheism, foundGraphics, notFoundAtheism, notFoundGraphics)
    }

    (nodes, documents)
  }
}

class Document(val label : Int) {
  private val words = Set[Int]()

  def addWord(wordId : Int) {
    if(wordId < 1) return
    words.add(wordId)
  }

  def containsWord(wordId : Int) = words.contains(wordId)
}