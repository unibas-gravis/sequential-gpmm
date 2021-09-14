package application.syntheticDataset

import breeze.linalg.Matrix
import method.evaluationTools.SegmentationSummary
import method.fittingTools.SequentialGpmmFitter
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.ScalarMeshField
import scalismo.geometry._3D
import scalismo.io.MeshIO
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.ui.api.{ScalismoUI, ScalismoUIHeadless, SimpleAPI}

import java.io.File

object Evaluation {

  import SequentialGpmmFitter._

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    Evaluation.run(SyntheticData.outputDirSeqGPMM)
    Evaluation.run(SyntheticData.outputDirRANSAC)
    Evaluation.run(SyntheticData.outputDirNRICP)
    Evaluation.run(SyntheticData.outputDirCPD)
  }

  def run(resultFilesLoc: File) = {
    val gtMapFiles = SyntheticData.groundTruthMaps.listFiles().sorted
    val gtTargetFile = SyntheticData.groundTruthMeshFile
    val gtTargetMesh = MeshIO.readMesh(gtTargetFile).get

    val resultFiles = resultFilesLoc.listFiles().toIndexedSeq

    val ui = ScalismoUI()
    ui.show(gtTargetMesh, "targetMesh")

    val allResults = gtMapFiles.map { gtMapFile =>
      evaluate(gtTargetMesh, resultFiles, gtMapFile, ui)
    }

    writeSummaryStatistic(allResults, resultFilesLoc, gtMapFiles)
  }

  private def evaluate(
      gtTargetMesh: TriangleMesh[_3D],
      resultFiles: IndexedSeq[File],
      gtMapFile: File,
      ui: SimpleAPI = ScalismoUIHeadless()
  ): Array[Double] = {
    val baseName = extractBasename(gtMapFile)
    println("++++++++++++++++++++++++++++++")
    println(s"evaluating $baseName ...")

    val gtMapOnReference = MeshIO.readScalarMeshField[Int](gtMapFile).get
    val (fittedMesh, fittedLabelMapOnReference, fittedLabelMapOnReferenceCleaned) = getFittingResults(resultFiles, baseName)

    val segmentationResults = SegmentationSummary(fittedLabelMapOnReference, gtMapOnReference)
    val segmentationResultsCleaned = SegmentationSummary(fittedLabelMapOnReferenceCleaned, gtMapOnReference)
    printSegmentationResults("segmentation results", segmentationResults)
    printSegmentationResults("segmentation results cleaned", segmentationResultsCleaned)

    val (hausdorffDistance, averageDistance, hausdorffDistanceHealthy, averageDistanceHealthy) = evaluateMeshMetrics(gtTargetMesh, gtMapOnReference, fittedMesh)
    printFittingResults(hausdorffDistance, averageDistance, hausdorffDistanceHealthy, averageDistanceHealthy)

    ui.show(fittedMesh, "fittedMesh")
    ui.show(gtMapOnReference, "gtMapOnReference")
    ui.show(fittedLabelMapOnReference, "fittedMapOnReference")
    ui.show(fittedLabelMapOnReferenceCleaned, "fittedLabelMapOnReferenceCleaned")

    Array(
      segmentationResults.f1Score,
      segmentationResults.truePositives,
      segmentationResults.trueNegatives,
      segmentationResults.falsePositives,
      segmentationResults.falseNegatives,
      segmentationResultsCleaned.f1Score,
      segmentationResultsCleaned.truePositives,
      segmentationResultsCleaned.trueNegatives,
      segmentationResultsCleaned.falsePositives,
      segmentationResultsCleaned.falseNegatives,
      averageDistance,
      averageDistanceHealthy,
      hausdorffDistance,
      hausdorffDistanceHealthy
    )
  }

  private def evaluateMeshMetrics(
      gtTargetMesh: TriangleMesh[_3D],
      gtMapOnReference: ScalarMeshField[Int],
      fittedMesh: TriangleMesh[_3D]
  ) = {
    val refPointsHealthy = gtMapOnReference.pointsWithValues.filter(_._2 == 0).map(_._1).toIndexedSeq
    val corrPointsHealthy = refPointsHealthy.map { pt =>
      gtTargetMesh.pointSet.findClosestPoint(pt).point
    }
    val refPointsHealthyBack = corrPointsHealthy.map { pt =>
      fittedMesh.pointSet.findClosestPoint(pt).point
    }
    val hausdorffDistance: Double = MeshMetrics.hausdorffDistance(fittedMesh, gtTargetMesh)
    val averageDistance: Double = MeshMetrics.avgDistance(fittedMesh, gtTargetMesh)
    val hausdorffDistanceHealthy = corrPointsHealthy.zip(refPointsHealthyBack).map { case (p1, p2) => (p1 - p2).norm }.max
    val averageDistanceHealthy = corrPointsHealthy.zip(refPointsHealthyBack).map { case (p1, p2)   => (p1 - p2).norm }.sum / corrPointsHealthy.length
    (hausdorffDistance, averageDistance, hausdorffDistanceHealthy, averageDistanceHealthy)
  }

  private def getFittingResults(
      resultDirectories: IndexedSeq[File],
      baseName: String
  ) = {
    val fittingResultDirectory = getFittingResultDirectory(resultDirectories, baseName)

    val fittedMesh = MeshIO.readMesh(new File(fittingResultDirectory, "fit.vtk")).get
    val labelMapOnReference = MeshIO.readScalarMeshField[Int](new File(fittingResultDirectory, "labelMapOnRef.vtk")).get

    val cleanedLabels = removeIsolatedOutliers(labelMapOnReference)
    val cleanedLabelMap = ScalarMeshField(fittedMesh, cleanedLabels)
    val cleanedLabelMapOnReference = ScalarMeshField(labelMapOnReference.mesh, cleanedLabels)

    MeshIO.writeScalarMeshField[Int](cleanedLabelMap, new File(fittingResultDirectory, "labelMapOnFitCleaned.vtk"))
    MeshIO.writeScalarMeshField[Int](cleanedLabelMapOnReference, new File(fittingResultDirectory, "labelMapOnRefCleaned.vtk"))

    (fittedMesh, labelMapOnReference, cleanedLabelMapOnReference)
  }

  private def removeIsolatedOutliers(labelMap: ScalarMeshField[Int], minRequiredOutlierNeighbours: Int = 0): IndexedSeq[Int] = {
    labelMap.pointsWithValues.map {
      case (point, value) =>
        if (value == Inlier) Inlier
        else {
          val pointId = labelMap.mesh.pointSet.pointId(point).get
          val neighboringPointIds = labelMap.mesh.triangulation.adjacentPointsForPoint(pointId)
          val neighborLabels = neighboringPointIds.map(labelMap(_))
          val nOutlierNeighbors = neighborLabels.count(_ == Outlier)
          if (nOutlierNeighbors >= minRequiredOutlierNeighbours) Outlier else Inlier
        }
    }.toIndexedSeq
  }

  private def getFittingResultDirectory(
      resultFiles: IndexedSeq[File],
      baseName: String
  ): File = {
    resultFiles.filter(_.getName.contains(baseName)).head
  }

  private def extractBasename(gtMapFile: File) = {
    gtMapFile.getName.replace("-onReference.vtk", "").replace(".stl", "")
  }

  private def writeSummaryStatistic(
      allResults: Array[Array[Double]],
      resultFilesLoc: File,
      gtMapFiles: Array[File]
  ) = {
    val m2 = Matrix.create[Double](allResults.head.length, gtMapFiles.length, allResults.flatten)
    breeze.linalg.csvwrite(new File(resultFilesLoc, "evaluationResults.csv"), m2)
  }

  private def printFittingResults(
      hausdorffDistance: Double,
      averageDistance: Double,
      hausdorffDistanceHealthy: Double,
      averageDistanceHealthy: Double
  ) = {
    println("reconstruction results")
    println(f"\t averageDistance:          $averageDistance")
    println(f"\t averageDistanceHealthy:   $averageDistanceHealthy")
    println(f"\t hausdorffDistance:        $hausdorffDistance")
    println(f"\t hausdorffDistanceHealthy: $hausdorffDistanceHealthy")
  }

  private def printSegmentationResults(title: String, summary: SegmentationSummary) = {
    println(title)
    println(f"\t f1Score:        ${summary.f1Score}")
    println(f"\t truePositives:  ${summary.truePositives}")
    println(f"\t trueNegatives:  ${summary.trueNegatives}")
    println(f"\t falsePositives: ${summary.falsePositives}")
    println(f"\t falseNegatives: ${summary.falseNegatives}")
  }
}
