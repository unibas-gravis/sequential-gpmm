package method.fittingTools

import java.io.File
import method.meshTools.ClosestPointFinders
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.{PointId, ScalarMeshField}
import scalismo.geometry.{Point, _3D}
import scalismo.io.MeshIO
import scalismo.mesh.TriangleMesh
import scalismo.numerics.UniformMeshSampler3D
import scalismo.statisticalmodel.PointDistributionModel
import scalismo.utils.Random.implicits._

object RansacFitter {

  def fitWithRansac(targetMesh: TriangleMesh[_3D],
                    initModel: PointDistributionModel[_3D, TriangleMesh],
                    targetOutputFolder: File,
                    randomSubsetFraction: Double,
                    consensusFraction: Double,
                    maxIterations: Int = 30,
                    inlierThreshold: Double = 5.0): ScalarMeshField[Int] = {

    val modelMean = initModel.mean

    val nModelPoints = modelMean.pointSet.numberOfPoints
    val subsetSize = (nModelPoints * randomSubsetFraction).toInt
    val consensusSize = (nModelPoints * consensusFraction).toInt
    println("subset and consensus", subsetSize, consensusSize)

    val fitResults = (0 until maxIterations).map { iteration =>
      val randomSubset = drawRandomSubset(modelMean, subsetSize)

      val correspondingPoints = getCorrespondences(randomSubset, modelMean, targetMesh)
      val posterior = initModel.posterior(correspondingPoints, 0.1)
      val (distances, targetPoints, consensusSet) = findConsensusSet(targetMesh, posterior, inlierThreshold)

      if (consensusSet.length > consensusSize) {
        println(s"accepted size: ${consensusSet.length}")
        val (posterior, averageCorrespondingDistance) = evaluateFullFit(consensusSet.zip(targetPoints), initModel, targetMesh)
        (posterior, consensusSet, averageCorrespondingDistance)
      } else {
        println("rejected")
        (posterior, randomSubset, Double.PositiveInfinity)
      }
    }

    val bestFit = fitResults.minBy(_._3)
    val (posterior, consensusSet, avgCorrespondenceDistance) = bestFit
    println(s"best avgCorrespondenceDistance $avgCorrespondenceDistance")
    println(s"consensus set size ${consensusSet.length}")

    val (bestLabelMap, bestLabelMapOnRef) = createLabelMaps(initModel, posterior, consensusSet)

    MeshIO.writeMesh(posterior.mean, new File(targetOutputFolder, "fit.vtk"))
    MeshIO.writeScalarMeshField[Int](bestLabelMap, new File(targetOutputFolder, "labelMap.vtk"))
    MeshIO.writeScalarMeshField[Int](bestLabelMapOnRef, new File(targetOutputFolder, "labelMapOnRef.vtk"))

    bestLabelMap
  }

  private def createLabelMaps(
      initModel: PointDistributionModel[_3D, TriangleMesh],
      posterior: PointDistributionModel[_3D, TriangleMesh],
      consensusSet: IndexedSeq[PointId]
  ) = {
    val labels = initModel.reference.pointSet.pointIds.toIndexedSeq.map { id =>
      if (consensusSet.contains(id)) SequentialGpmmFitter.Inlier else SequentialGpmmFitter.Outlier
    }
    val bestLabelMap = ScalarMeshField[Int](posterior.mean, labels)
    val bestLabelMapOnRef = ScalarMeshField[Int](initModel.reference, labels)
    (bestLabelMap, bestLabelMapOnRef)
  }

  private def evaluateFullFit(
      correspondences: IndexedSeq[(PointId, Point[_3D])],
      initModel: PointDistributionModel[_3D, TriangleMesh],
      targetMesh: TriangleMesh[_3D]
  ) = {
    val posterior = initModel.posterior(correspondences, 0.1)
    val averageDistance = averageCorrespondingDistance(targetMesh, posterior)
    (posterior, averageDistance)
  }

  private def averageCorrespondingDistance(targetMesh: TriangleMesh[_3D], posterior: PointDistributionModel[_3D, TriangleMesh]) = {
    val dsnew = posterior.mean.pointSet.pointsWithId.map {
      case (point, pointId) =>
        val normal = posterior.mean.vertexNormals(pointId)
        val targetPoint = ClosestPointFinders.getClosestPointIncludingNormal(targetMesh, point, normal)._1
        (targetPoint - point).norm
    }.toIndexedSeq
    val adnew = dsnew.sum / dsnew.length
    adnew
  }

  private def findConsensusSet(targetMesh: TriangleMesh[_3D], posterior: PointDistributionModel[_3D, TriangleMesh], inlierThreshold: Double) = {
    val fitPairs = posterior.mean.pointSet.pointsWithId.map {
      case (pt, id) =>
        val normal = posterior.mean.vertexNormals.atPoint(id)
        val targetPoint = ClosestPointFinders.getClosestPointIncludingNormal(targetMesh, pt, normal)._1
        ((pt - targetPoint).norm, targetPoint, id)
    }.toIndexedSeq
    val acceptedPairs = fitPairs.filter(_._1 < inlierThreshold)
    acceptedPairs.unzip3
  }

  private def getCorrespondences(randomSubset: IndexedSeq[PointId], modelMean: TriangleMesh[_3D], targetMesh: TriangleMesh[_3D]) = {
    val newPointsTarget = randomSubset.map { id =>
      val point = modelMean.pointSet.point(id)
      val normal = modelMean.vertexNormals.atPoint(id)
      ClosestPointFinders.getClosestPointIncludingNormal(targetMesh, point, normal)._1
    }
    val randomPairs = randomSubset.zip(newPointsTarget)
    randomPairs
  }

  private def drawRandomSubset(
      modelMean: TriangleMesh[_3D],
      subsetSize: Int
  ) = {
    val randomPoints = UniformMeshSampler3D(modelMean, subsetSize).sample().map(_._1)
    val newIdsSample = randomPoints.map(modelMean.pointSet.findClosestPoint(_).id)
    newIdsSample
  }
}
