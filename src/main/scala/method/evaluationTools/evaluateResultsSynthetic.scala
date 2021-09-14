package method.evaluationTools

import scalismo.common.DiscreteField.ScalarMeshField

case class SegmentationSummary(
    truePositives: Int,
    trueNegatives: Int,
    falsePositives: Int,
    falseNegatives: Int
) {
  val nPoints: Int = truePositives + trueNegatives + falsePositives + falseNegatives

  val precision: Double = truePositives.toDouble / (falsePositives + truePositives)
  val recall: Double = truePositives.toDouble / (falseNegatives + truePositives)

  val accuracy: Double = (trueNegatives + truePositives).toDouble / nPoints
  val prevalence: Double = (falseNegatives + truePositives).toDouble / nPoints

  val missClassificationRate: Double = 1.0 - accuracy

  val harmonicAverage = ((1 / precision) + (1 / recall)) / 2
  val f1Score: Double = 1.0 / harmonicAverage

  val MCC: Double = (truePositives * trueNegatives - falsePositives * falseNegatives).toDouble / math.sqrt(
    (truePositives + falsePositives) * (truePositives + falseNegatives) * (trueNegatives + falsePositives) * (trueNegatives + falseNegatives)
  )
}

object SegmentationSummary {

  def apply(groundTruth: ScalarMeshField[Int], predicted: ScalarMeshField[Int]): SegmentationSummary = {
    val values = groundTruth.data.zip(predicted.data)

    val truePositives = values.count { case (gt: Int, pred: Int)  => gt == 1 && pred == 1 }
    val trueNegatives = values.count { case (gt: Int, pred: Int)  => gt == 0 && pred == 0 }
    val falsePositives = values.count { case (gt: Int, pred: Int) => gt == 0 && pred == 1 }
    val falseNegatives = values.count { case (gt: Int, pred: Int) => gt == 1 && pred == 0 }

    new SegmentationSummary(
      truePositives,
      trueNegatives,
      falsePositives,
      falseNegatives
    )
  }

}
