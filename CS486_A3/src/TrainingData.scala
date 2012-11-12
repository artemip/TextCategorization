import io.Source

object TrainingData {
  val (documents, words) = {
    val documentLines = Source.fromFile("data/testData.txt").getLines.toList
    val categoryLines = Source.fromFile("data/testLabel.txt").getLines.toList
    val wordLines = Source.fromFile("data/words.txt").getLines.toList

    val numWords = wordLines.size

    val categoryIterator = categoryLines.iterator
    var category = -1

    var currentDoc : Document = null
    var currentDocId = -1

    var documents = List[Document]()

    for(docLine <- documentLines) {
      val (docId, wordId) = {
        val splitLine = docLine.split("\t")
        if (splitLine.size > 0) (splitLine.head.toInt, splitLine.last.toInt) else (-1, -1)
      }

      if(docId != currentDocId) {
        if(currentDoc != null) documents = currentDoc :: documents
        if(categoryIterator.hasNext) {
          category = categoryIterator.next().toInt
          currentDoc = new Document(numWords, category)
        }
      }

      currentDoc.foundWord(wordId)
    }

    var index = -1

    val words = {
      for(wordLine <- wordLines) yield {
        index = index + 1

        var found = List[Outcome]()
        var notFound = List[Outcome]()

        for(doc <- documents) {
          if (doc.getWord(index + 1)) { //Word found
            found = Outcome(doc.category == 1) :: found
          } else {
            notFound = Outcome(doc.category == 1) :: notFound
          }
        }

        new Word(wordLine, found, notFound)
      }
    }.toList

    (documents, words)
  }
}

class Document(numWords : Int, val category : Int) {
  private val words = Array.fill[Boolean](numWords)(false)

  def foundWord(wordId : Int) {
    if(wordId < 1) return

    words(wordId - 1) = true
  }

  def getWord(wordId : Int) = words(wordId - 1)
}