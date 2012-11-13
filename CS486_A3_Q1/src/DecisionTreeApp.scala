import org.jfree.ui.RefineryUtilities

object DecisionTreeApp extends App {
  var extendedInformationGain = false

  /*
  val tree = DecisionTree(10, TrainingData.sortedNodesQueue)
  tree.print()

  extendedInformationGain = true
  val extendedTree = DecisionTree(10, TrainingData.sortedNodesQueue)
  extendedTree.print()
*/

  val plotter = new Plotter("Decision Tree Results")
  plotter.pack()
  RefineryUtilities.centerFrameOnScreen(plotter)
  plotter.setVisible(true)

}
