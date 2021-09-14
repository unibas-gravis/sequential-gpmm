package method.meshTools

import java.io.File

import application.syntheticDataset.SyntheticData
import scalismo.io.{MeshIO, StatismoIO, StatisticalModelIO}

import scala.util.Random

object ClippedDatasetGenerator {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val meshDir = new File("/export/skulls/projects/occlusion/mandible/combinedModel/registered-data")

    val model = StatisticalModelIO.readStatisticalTriangleMeshModel3D(SyntheticData.modelFile).get

    val nameOfGroundTruthTarget = "shapePoseMeshToClip"
    val targetMeshWithPose = MeshIO.readMesh(meshDir.listFiles().filter(_.getName.contains(nameOfGroundTruthTarget)).head).get

    val clippedDatasetDir = new File(meshDir, "syntheticClippedDataset/downsampledShapePose")
    clippedDatasetDir.mkdirs()

    val nEachSize = 10

    val nTotalPoints = targetMeshWithPose.pointSet.pointIds.length

    val nPointsToRemove = Seq(
      5 * nTotalPoints / 6,
      3 * nTotalPoints / 4,
      nTotalPoints / 2,
      nTotalPoints / 3,
      nTotalPoints / 4,
      nTotalPoints / 5,
      nTotalPoints / 10,
      nTotalPoints / 20,
      nTotalPoints / 50,
      nTotalPoints / 100
    )

    nPointsToRemove.foreach { nPoints =>
      (0 until nEachSize).foreach { i =>
        val randomInt = Random.nextInt(nTotalPoints)
        val center = targetMeshWithPose.pointSet.points.toIndexedSeq(randomInt)
        val clippedMeshWithIds = MeshClipper.usingSphereWithIds(targetMeshWithPose, center, nPoints)
        val clippedMesh = clippedMeshWithIds._1
        val name = s"removed$nPoints-$i"
        MeshIO.writeMesh(clippedMesh, new File(clippedDatasetDir, name + ".stl"))
        val idsToKeep = clippedMeshWithIds._2
        val pointsToKeepOnRef = idsToKeep.map {
          model.reference.pointSet.point(_)
        }
        val fullMapRef = MeshLabeler.labelForegroundPoints(model.reference, pointsToKeepOnRef)
        MeshIO.writeScalarMeshField[Int](fullMapRef, new File(clippedDatasetDir, s"field-$name-onReference.vtk")).get

      }

    }

  }

}
