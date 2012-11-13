import collection.mutable
import collection.mutable.ListBuffer
import collection.mutable.Set
import io.Source

object TrainingData {

  def extractDocuments(training : Boolean) = {
    val (documentPath, labelPath) = if(training) ("data/trainData.txt", "data/trainLabel.txt") else ("data/testData.txt", "data/testLabel.txt")
    val documentLines = Source.fromFile(documentPath).getLines.toList
    val labelLines = Source.fromFile(labelPath).getLines.toList

    val labelIterator = labelLines.iterator
    var label = -1

    //Get first document
    val firstDoc = documentLines.head
    val (firstDocId, firstDocWordId) = {
      val splitLine = firstDoc.split("\t")
      if (splitLine.size > 0) (splitLine.head.toInt, splitLine.last.toInt) else (-1, -1)
    }
    var currentDoc : Document = new Document(labelIterator.next().toInt)
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
        if(labelIterator.hasNext) {
          label = labelIterator.next().toInt
          currentDoc = new Document(label)
          currentDocId = docId
        }
      }

      currentDoc.addWord(wordId)
    }

    documents
  }

  val (wordAttributes, trainDocuments) = {
    val documents = extractDocuments(true)
    val wordLines = Source.fromFile("data/words.txt").getLines.toList
    val attributes = ListBuffer[Attribute]()

    var index = 0

    for(wordLine <- wordLines) {
      index = index + 1

      var foundAtheism, foundGraphics = 0

      for(doc <- documents) {
        if (doc.containsWord(index)) { //Word found
          if(doc.label == 1) foundAtheism = foundAtheism + 1
          else foundGraphics = foundGraphics + 1
        }
      }

      attributes prepend new Attribute(wordLine, index, foundAtheism, foundGraphics)
    }

    (attributes, documents)
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

