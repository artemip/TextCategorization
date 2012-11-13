import java.awt.Color
import org.jfree.chart.{JFreeChart, ChartFactory, ChartPanel}
import org.jfree.chart.axis.{NumberTickUnit, NumberAxis}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.ui.ApplicationFrame

class Plotter(title : String) extends ApplicationFrame(title) {

  val dataset = createDataSet()
  val chart = createChart(dataset)

  val chartPanel = new ChartPanel(chart)

  chartPanel.setPreferredSize(new java.awt.Dimension(500, 270))
  setContentPane(chartPanel)


  def createDataSet() : XYDataset = {

    def getLabelId(name : String) = {
      name match {
        case "alt.atheism" => 1
        case "comp.graphics" => 2
      }
    }

    val testSeries = new XYSeries("Test Set")
    val trainingSeries = new XYSeries("Training Set")

    val testDocuments = TrainingData.extractDocuments(false)
    val numTestDocuments = testDocuments.size

    val trainingDocuments = TrainingData.trainDocuments
    val numTrainingDocuments = trainingDocuments.size

    for(numNodes <- 1 to 100) {
      val tree = DecisionTree(numNodes, TrainingData.sortedNodesQueue)

      //Training set
      var numCorrectlyClassified = 0

      for(doc <- trainingDocuments) {
        val classification = tree.classify(doc)
        if(getLabelId(classification) == doc.label) {
          numCorrectlyClassified = numCorrectlyClassified + 1
        }
      }

      trainingSeries.add(numNodes, numCorrectlyClassified.toDouble / numTrainingDocuments)

      //Test set
      numCorrectlyClassified = 0

      for(doc <- testDocuments) {
        val classification = tree.classify(doc)
        if(getLabelId(classification) == doc.label) {
          numCorrectlyClassified = numCorrectlyClassified + 1
        }
      }

      testSeries.add(numNodes, numCorrectlyClassified.toDouble / numTestDocuments)
    }

    val dataset = new XYSeriesCollection()
    dataset.addSeries(testSeries)
    dataset.addSeries(trainingSeries)

    dataset
  }

  private def createChart(dataset : XYDataset) : JFreeChart = {

    val chart = ChartFactory.createXYLineChart(
      "",      // chart title
      "# Nodes",                      // x axis label
      "% Correct",                      // y axis label
      dataset,                  // data
      PlotOrientation.VERTICAL,
      true,                     // include legend
      true,                     // tooltips
      false                     // urls
    )

    chart.setBackgroundPaint(Color.white)

    val plot = chart.getXYPlot
    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinePaint(Color.white)
    plot.setRangeGridlinePaint(Color.white)
    plot.setDomainCrosshairVisible(true)
    plot.setRangeCrosshairVisible(true)

    val renderer = new XYLineAndShapeRenderer()
    renderer.setSeriesLinesVisible(0, true)
    renderer.setSeriesLinesVisible(1, true)
    //renderer.setSeriesShapesVisible(0, false)
    //renderer.setSeriesShapesVisible(1, false)
    plot.setRenderer(renderer)

    val domain = plot.getDomainAxis.asInstanceOf[NumberAxis]
    domain.setRange(1, 105)
    //domain.setAutoRange(true)

    val range = plot.getRangeAxis.asInstanceOf[NumberAxis]
    range.setRange(0.5, 0.85)
    //range.setAutoRange(true)

    chart
  }
}
