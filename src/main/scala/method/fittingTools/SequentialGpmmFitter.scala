package method.fittingTools

import breeze.linalg.{DenseVector, Matrix}
import breeze.numerics.{exp, log}
import breeze.stats.distributions.Gaussian
import com.cibo.evilplot.colors.HTMLNamedColors.{blue, red}
import com.cibo.evilplot.geometry.Extent
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.BarRenderer
import method.meshTools.{ClosestPointFinders, MeshLabeler}
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.{PointId, ScalarMeshField}
import scalismo.geometry.{EuclideanVector, Landmark, Point, _3D}
import scalismo.io.{MeshIO, StatisticalModelIO}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel}

import java.io.File
import scala.util.Try

object SequentialGpmmFitter {
  final val Inlier = 0
  final val Outlier = 1

  final val AlreadyInlier = 0
  final val NotEvalutedOutlier = 1
  final val NewInlier = 2
  final val RejectedOutlier = 3
}

case class SequentialGpmmFitter(
    target: TriangleMesh[_3D],
    model: PointDistributionModel[_3D, TriangleMesh],
    targetOutputFolder: File,
    correspondenceFunction: (TriangleMesh[_3D], Point[_3D], EuclideanVector[_3D]) => (Point[_3D], EuclideanVector[_3D]) =
      ClosestPointFinders.getClosestPointAlongIntersection
) {
  import SequentialGpmmFitter._

  def runSequentialGP(
      landmarks: IndexedSeq[Landmark[_3D]],
      confidenceLevel: Double = 0.5,
      sigmaSquaredNoise: Double = 1.0,
      writeDebugOutput: Boolean = false,
      landmarksTarget: Option[IndexedSeq[Landmark[_3D]]] = None
  ): ScalarMeshField[Int] = {
    writeStartingState(model, target)
    val initialState = initialFittingState(landmarks, landmarksTarget)
    val fitIterator = Iterator.iterate(initialState) { state =>
      fittingStep(state, confidenceLevel, sigmaSquaredNoise, writeDebugOutput)
    }
    val (convergence, convergedStates) = fitIterator.span(_.newlyAddedInlierCount != 0)
    val fittingResult = convergedStates.next()
    write(fittingResult)
    println(s"niter ${fittingResult.iteration}")

    if (writeDebugOutput) {
      writeFitDebugInformation(fittingResult, convergence)
    }

    fittingResult.labelMapOnFit
  }

  private def initialFittingState(landmarks: IndexedSeq[Landmark[_3D]], landmarksTarget: Option[IndexedSeq[Landmark[_3D]]]): FittingState = {
    val landmarkIds = landmarks.map(lm => model.reference.pointSet.findClosestPoint(lm.point).id)
    val labelMapOnFit = MeshLabeler.labelForegroundPointsFromIDs(model.reference, landmarkIds)

    val correspondences = if (landmarksTarget.isDefined) {
      landmarkIds.zip(landmarksTarget.get.map {
        _.point
      })
    } else {
      estimateDoubleProjectedCorrespondences(landmarkIds, model, target)
    }
    val labelMapOnTarget = MeshLabeler.labelForegroundPoints(target, correspondences.map(_._2))

    val mhDistances = model.mean.pointSet.pointIds.map(getResidualAndMahalanobisDistance(target, model, _)._2).toIndexedSeq
    val mhDistanceMap = ScalarMeshField[Double](model.mean, mhDistances)

    FittingState(
      iteration = 0,
      inlierCount = landmarkIds.length,
      newlyAddedInlierCount = landmarkIds.length,
      posterior = model,
      correspondences = correspondences,
      labelMapOnFit = labelMapOnFit,
      labelMapOnTarget = labelMapOnTarget,
      mhDistanceMap = mhDistanceMap,
      labelChanges = labelMapOnFit.values.toIndexedSeq
    )
  }

  private def fittingStep(
      current: FittingState,
      confidenceLevel: Double,
      sigmaNoise: Double,
      debugOutput: Boolean = false
  ): FittingState = {
    val (currentInlierIds, _) = getInlierAndOutlierIds(current.labelMapOnFit)

    val iteration = current.iteration + 1
    val posterior = model.posterior(current.correspondences, sigmaNoise) // note: important to use here the model not the state.model
    val fit = posterior.mean

    val (residuals, mhDistances) = getResidualsAndMahalanobisDistances(target, posterior)

    val inlierMHDistances = currentInlierIds.map(pid => mhDistances(pid.id))
    val threshold: Double = calculateThreshold(inlierMHDistances, confidenceLevel)
    println(s"logdata domain threshold $threshold ")

    val labelMapOnNewFit = ScalarMeshField(fit, current.labelMapOnFit.data)

    val mhDistanceMap = ScalarMeshField[Double](fit, mhDistances)
    val (rgResult, rgHistory) = regionGrowing(labelMapOnNewFit, mhDistanceMap, threshold)
    val rgStates = (rgHistory.toSeq :+ rgResult).toIndexedSeq
    val labelMap = rgResult.labelMap

    if (debugOutput) {
      val debugDir = new File(targetOutputFolder, "debug-plots")
      if (!debugDir.exists()) debugDir.mkdirs()

      val inlierResiduals = currentInlierIds.map(pid => residuals(pid.id))

      writeRegionGrowingDebugInformation(rgStates, mhDistances, residuals, threshold, iteration, debugDir)
      writeQqPlotsVsNormal(inlierMHDistances, inlierResiduals, iteration, debugDir)
    }

    val inlierPositions = labelMap.pointsWithValues.toIndexedSeq.filter(_._2 == Inlier).map(_._1)
    val inlierCount = inlierPositions.length
    val inlierIds = inlierPositions.map(pt => labelMap.mesh.pointSet.pointId(pt).get)

    val correspondences = estimateDoubleProjectedCorrespondences(inlierIds, posterior, target)
    val labelMapOnTarget = MeshLabeler.labelForegroundPoints(target, correspondences.map(_._2))

    FittingState(
      iteration = iteration,
      inlierCount = inlierCount,
      newlyAddedInlierCount = inlierCount - current.inlierCount,
      posterior = posterior,
      correspondences = correspondences,
      labelMapOnFit = labelMap,
      labelMapOnTarget = labelMapOnTarget,
      mhDistanceMap = mhDistanceMap,
      labelChanges = summarizeLabelChanges(rgStates)
    )
  }

  private def regionGrowing(
      inlierMap: ScalarMeshField[Int],
      mhDistanceMap: ScalarMeshField[Double],
      threshold: Double
  ): (LabelMapState, Iterator[LabelMapState]) = {
    val initialInlierCount = inlierMap.data.count(_ == Inlier)

    val initialState = LabelMapState(
      iteration = 0,
      inlierCount = initialInlierCount,
      newlyAddedInliers = initialInlierCount,
      labelMap = inlierMap,
      category = inlierMap.values.toIndexedSeq
    )

    val labelMapStates = Iterator.iterate(initialState) { state =>
      val (newLabels, category) = regionGrowingStep(state.labelMap, mhDistanceMap, threshold).unzip
      val newInlierCount = newLabels.count(_ == Inlier)

      LabelMapState(
        iteration = state.iteration + 1,
        inlierCount = newInlierCount,
        newlyAddedInliers = newInlierCount - state.inlierCount,
        labelMap = ScalarMeshField[Int](state.labelMap.domain, newLabels),
        category = category
      )
    }

    val (convergence, convergedStates) = labelMapStates.span(_.newlyAddedInliers != 0)
    val result = convergedStates.next()
    (result, convergence)
  }

  private def regionGrowingStep(
      inlierMap: ScalarMeshField[Int],
      mhDistanceMap: ScalarMeshField[Double],
      threshold: Double
  ): IndexedSeq[(Int, Int)] = {
    inlierMap.domain.pointSet.pointIds.map { id =>
      val isInlier = inlierMap(id)
      val hasInlierNeighbour = inlierNeighbourExists(inlierMap, id)
      val distanceBelowThreshold = log(mhDistanceMap(id)) < threshold
      (isInlier, hasInlierNeighbour, distanceBelowThreshold) match {
        case (Inlier, _, _)         => (Inlier, AlreadyInlier)
        case (Outlier, true, true)  => (Inlier, NewInlier)
        case (Outlier, true, false) => (Outlier, RejectedOutlier)
        case _                      => (Outlier, NotEvalutedOutlier)
      }
    }.toIndexedSeq
  }

  private def inlierNeighbourExists(
      inlierMap: ScalarMeshField[Int],
      id: PointId
  ): Boolean = {
    val adjacentPoints = inlierMap.domain.triangulation.adjacentPointsForPoint(id)
    adjacentPoints.exists(id => inlierMap(id) == Inlier)
  }

  private def getInlierAndOutlierIds(inlierMap: ScalarMeshField[Int]) = {
    val iterators = inlierMap.domain.pointSet.pointIds.partition(inlierMap(_) == Inlier)
    (iterators._1.toIndexedSeq, iterators._2.toIndexedSeq)
  }

  private def getResidualsAndMahalanobisDistances(targetMesh: TriangleMesh[_3D], posterior: PointDistributionModel[_3D, TriangleMesh]) = {
    posterior.reference.pointSet.pointIds
      .map(id => getResidualAndMahalanobisDistance(targetMesh, posterior, id))
      .toIndexedSeq
      .unzip
  }

  private def getResidualsAndMahalanobisDistances(targetMesh: TriangleMesh[_3D],
                                                  pointIds: IndexedSeq[PointId],
                                                  posterior: PointDistributionModel[_3D, TriangleMesh]) = {
    pointIds.map { id =>
      getResidualAndMahalanobisDistance(targetMesh, posterior, id)
    }.unzip
  }

  private def getResidualAndMahalanobisDistance(
      targetMesh: TriangleMesh[_3D],
      posterior: PointDistributionModel[_3D, TriangleMesh],
      id: PointId
  ) = {
    val mean = posterior.mean
    val point = mean.pointSet.point(id)
    val normal = mean.vertexNormals.atPoint(id)
    val ptOnTarget = correspondenceFunction(targetMesh, point, normal)._1
    val residual = (ptOnTarget - point).norm

    val cov = posterior.cov(id, id)
    val id_distribution = MultivariateNormalDistribution(DenseVector(point.toArray), cov)
    val mahalanobisDistance = id_distribution.mahalanobisDistance(DenseVector(ptOnTarget.toArray))

    (residual, mahalanobisDistance)
  }

  private def calculateThreshold(
      mahalanobisDistances: IndexedSeq[Double],
      confidenceLevel: Double
  ) = {
    // get threshold using inlier logdistances
    val dataForClassification = mahalanobisDistances.map(log(_))
    val inlierDistributionMean = dataForClassification.sum / dataForClassification.length
    val inlierDistributionVar = dataForClassification.map { d =>
      (d - inlierDistributionMean) * (d - inlierDistributionMean)
    }.sum / dataForClassification.length
    val threshold = Gaussian.distribution(inlierDistributionMean, inlierDistributionVar).inverseCdf(1.0 - (1.0 - confidenceLevel) / 2.0)
    threshold
  }

  private def estimateDoubleProjectedCorrespondences(inlierIds: IndexedSeq[PointId],
                                                     model: PointDistributionModel[_3D, TriangleMesh],
                                                     target: TriangleMesh[_3D]) = {
    val (targetPoints, targetNormals) = inlierIds.map { inlierId =>
      val point = model.mean.pointSet.point(inlierId)
      val normal = model.mean.vertexNormals.atPoint(inlierId)
      correspondenceFunction(target, point, normal)
    }.unzip

    val (modelPointsDP, _) = targetPoints.zip(targetNormals).map { case (point, normal) => correspondenceFunction(model.mean, point, normal) }.unzip

    val doubleProjectionIDs = modelPointsDP.map { pt =>
      model.mean.pointSet.findClosestPoint(pt).id
    }

    doubleProjectionIDs.zip(targetPoints)
  }

  private def summarizeLabelChanges(states: IndexedSeq[LabelMapState]): scala.IndexedSeq[Int] = {
    val categories = states.map(_.category)
    for (i <- categories.head.indices) yield {
      val historyForSinglePoint = categories.map(_(i))
      if (historyForSinglePoint.contains(NewInlier)) NewInlier
      else if (historyForSinglePoint.contains(RejectedOutlier)) RejectedOutlier
      else historyForSinglePoint.head
    }
  }

  private def writeStartingState(
      initialModel: PointDistributionModel[_3D, TriangleMesh],
      target: TriangleMesh[_3D]
  ): Try[Unit] = {
    StatisticalModelIO.writeStatisticalTriangleMeshModel3D(initialModel, new File(targetOutputFolder, "initModel.h5"))
    MeshIO.writeMesh(target, new File(targetOutputFolder, "target.vtk"))
  }

  private def writeFitDebugInformation(
      fit: FittingState,
      history: Iterator[FittingState]
  ): Unit = {
    def avg(values: IndexedSeq[Double]): Double = values.sum / values.length

    val states = history
      .map(fs => (fs.inlierCount, fs.mhDistanceMap, fs.labelChanges))
      .toIndexedSeq :+ (fit.inlierCount, fit.mhDistanceMap, fit.labelChanges)

    val tuples = states.map {
      case (_, mhDistances, updateLabelCategory) =>
        val values = updateLabelCategory.zip(mhDistances.values)
        val existingInlierMhDistances = values.filter(_._1 == AlreadyInlier).map(_._2)
        val newInlierMhDistances = values.filter(_._1 == NewInlier).map(_._2)
        val rejectedOutlierMhDistances = values.filter(_._1 == RejectedOutlier).map(_._2)
        val notEvaluatedOutlierMhDistances = values.filter(_._1 == NotEvalutedOutlier).map(_._2)
        (avg(existingInlierMhDistances), avg(newInlierMhDistances), avg(rejectedOutlierMhDistances), avg(notEvaluatedOutlierMhDistances))
    }

    val existingInlierMhDistanceAverage = tuples.map(_._1)
    val newInlierMhDistancesAverage = tuples.map(_._2)
    val stillOutlierMhDistancesAverage = tuples.map(_._3)
    val notEvaluatedOutlierMhDistances = tuples.map(_._4)

    writeList(states.map(_._1), new File(targetOutputFolder, "nInlierList.csv"))
    writeList(existingInlierMhDistanceAverage, new File(targetOutputFolder, "averageExistingInlierMhDistances.csv"))
    writeList(newInlierMhDistancesAverage, new File(targetOutputFolder, "averageNewInlierMhDistances.csv"))
    writeList(stillOutlierMhDistancesAverage, new File(targetOutputFolder, "averageRejectedOutlierMhDistances.csv"))
    writeList(notEvaluatedOutlierMhDistances, new File(targetOutputFolder, "averageNotConsideredOutlierMHDistances.csv"))
  }

  def writeList(
      value: IndexedSeq[Double],
      file: File
  ): Unit = {
    val data = Matrix(value)
    breeze.linalg.csvwrite(file, data)
  }

  def writeRegionGrowingDebugInformation(
      history: Seq[LabelMapState],
      mhDistances: Seq[Double],
      residuals: Seq[Double],
      threshold: Double,
      outerIteration: Int,
      debugDirectory: File
  ): Unit = {
    if (!debugDirectory.exists()) debugDirectory.mkdirs()

    for (state <- history) {
      val baseFilename = f"$outerIteration%04d_${state.iteration}%04d_${state.inlierCount}"

      val inlierMHDistances = state.labelMap.valuesWithIds.filter(_._1 == Inlier).map(vid => mhDistances(vid._2.id)).toIndexedSeq
      val outlierMHDistances = state.labelMap.valuesWithIds.filter(_._1 == Outlier).map(vid => mhDistances(vid._2.id)).toIndexedSeq
      val inlierResiduals = state.labelMap.valuesWithIds.filter(_._1 == Inlier).map(vid => residuals(vid._2.id)).toIndexedSeq
      val outlierResiduals = state.labelMap.valuesWithIds.filter(_._1 == Outlier).map(vid => residuals(vid._2.id)).toIndexedSeq

      plotHistogramWithThreshold(inlierMHDistances, outlierMHDistances, exp(threshold), new File(debugDirectory, s"${baseFilename}_histogram.png"))
      plotHistogramWithThreshold(inlierMHDistances.map(log(_)),
                                 outlierMHDistances.map(log(_)),
                                 threshold,
                                 new File(debugDirectory, s"${baseFilename}_histogram_log.png"))
      plotHistogram(inlierResiduals, outlierResiduals, new File(debugDirectory, s"${baseFilename}_histogram_residuals.png"))
    }
  }

  def plotHistogramWithThreshold(
      inlier: Seq[Double],
      outlier: Seq[Double],
      threshold: Double,
      file: File,
      plotAreaSize: Extent = Extent(1000, 600)
  ): Unit = {
    Overlay(
      Histogram(inlier, barRenderer = Some(BarRenderer.default(Some(blue.copy(opacity = 0.5))))),
      Histogram(outlier, barRenderer = Some(BarRenderer.default(Some(red.copy(opacity = 0.5))))),
      ScatterPlot(Seq(com.cibo.evilplot.numeric.Point(threshold, 0.0))).vline(threshold)
    ).standard()
      .render(plotAreaSize)
      .write(file)
  }

  def plotHistogram(
      inlier: Seq[Double],
      outlier: Seq[Double],
      file: File,
      plotAreaSize: Extent = Extent(1000, 600)
  ): Unit = {
    Overlay(
      Histogram(inlier, barRenderer = Some(BarRenderer.default(Some(blue.copy(opacity = 0.5))))),
      Histogram(outlier, barRenderer = Some(BarRenderer.default(Some(red.copy(opacity = 0.5)))))
    ).standard()
      .render(plotAreaSize)
      .write(file)
  }

  private def writeQqPlotsVsNormal(
      mhDistances: IndexedSeq[Double],
      residuals: IndexedSeq[Double],
      iteration: Int,
      plottingDir: File
  ): Unit = {
    val sortedMhDistances = mhDistances.sorted
    val sortedResiduals = residuals.sorted

    val n = sortedMhDistances.length
    assert(sortedResiduals.length == n)

    val percentiles = (0 until n).map(i => (i + 1.0) / (n + 1))

    val stdNormal = breeze.stats.distributions.Gaussian(0.0, 1.0)
    val quantilesNormal = percentiles.map(p => stdNormal.inverseCdf(p))

    val logNormal = breeze.stats.distributions.LogNormal(0.0, 1.0)
    val quantilesLogNormal = percentiles.map(p => logNormal.inverseCdf(p))

    val logMhDistances = sortedMhDistances.map(log(_))

    qqPlot(quantilesLogNormal, sortedMhDistances, new File(plottingDir, f"$iteration%04d_qqplot_DataVsLogNormal.png"))
    qqPlot(quantilesNormal, logMhDistances, new File(plottingDir, f"$iteration%04d_qqplot-logdataVsNormal.png"))
    qqPlot(quantilesNormal, sortedResiduals, new File(plottingDir, f"$iteration%04d_qqplot-residualsVsNormal.png"))
  }

  def qqPlot(x: IndexedSeq[Double], y: IndexedSeq[Double], file: File, plotAreaSize: Extent = Extent(1000, 600)): Unit = {
    Overlay(ScatterPlot(x.zip(y).map { case (x, y) => com.cibo.evilplot.numeric.Point(x, y) }))
      .standard()
      .xLabel("x")
      .yLabel("y")
      .rightLegend()
      .render(plotAreaSize)
      .write(file)
  }

  def write(state: FittingState): Unit = {
    StatisticalModelIO.writeStatisticalTriangleMeshModel3D(state.posterior, new File(targetOutputFolder, "posterior.h5"))

    MeshIO.writeMesh(state.posterior.mean, new File(targetOutputFolder, "fit.vtk"))
    MeshIO.writeScalarMeshField[Int](state.labelMapOnFit, new File(targetOutputFolder, "labelMapOnFit.vtk"))
    MeshIO.writeScalarMeshField[Double](state.mhDistanceMap, new File(targetOutputFolder, "mhDistanceSquaredOnFit.vtk"))

    MeshIO.writeScalarMeshField[Int](state.labelMapOnTarget, new File(targetOutputFolder, "labelMapOnTarget.vtk"))

    val labelMapOnRef = ScalarMeshField[Int](state.posterior.reference, state.labelMapOnFit.data)
    MeshIO.writeScalarMeshField[Int](labelMapOnRef, new File(targetOutputFolder, "labelMapOnRef.vtk"))
    val mhDistanceOnRef = ScalarMeshField[Double](state.posterior.reference, state.mhDistanceMap.data)
    MeshIO.writeScalarMeshField[Double](mhDistanceOnRef, new File(targetOutputFolder, "mhDistanceSquaredOnRef.vtk"))
  }

  case class LabelMapState(
      iteration: Int,
      inlierCount: Int,
      newlyAddedInliers: Int,
      labelMap: ScalarMeshField[Int],
      category: IndexedSeq[Int]
  )

  case class FittingState(iteration: Int,
                          inlierCount: Int,
                          newlyAddedInlierCount: Int,
                          posterior: PointDistributionModel[_3D, TriangleMesh],
                          correspondences: IndexedSeq[(PointId, Point[_3D])],
                          labelMapOnFit: ScalarMeshField[Int],
                          labelMapOnTarget: ScalarMeshField[Int],
                          mhDistanceMap: ScalarMeshField[Double],
                          labelChanges: IndexedSeq[Int])

}
