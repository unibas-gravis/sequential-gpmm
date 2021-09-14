package application.KITS

import java.io.File

import application.DataDirectory
import method.evaluationTools.SegmentationSummary
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.geometry._3D
import scalismo.io.MeshIO
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.utils.Random.implicits._

object evaluate {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val gtmaplocation = KitsData.gtmaplocation

    val names = gtmaplocation.listFiles().filterNot(_.getName.contains("case_00000")).sorted.map(_.getName.replace(".vtk", "")).take(200)

    var avgTumor = Array.empty[Double]
    var avgKidney = Array.empty[Double]
    var avgF1 = Array.empty[Double]
    var avgMCC = Array.empty[Double]

    names

      .foreach { name =>
        println(s"*********** $name ***************")

        val labelMapOnGTshapemap = new File(KitsData.outputDirSeqGPMMkits, s"mesh$name/labelMapOnGTshape.vtk")
        val reconstructionmap = new File(KitsData.outputDirSeqGPMMkits, s"mesh$name/labelMapOnFit.vtk")
        val gtmap = new File(KitsData.outputDirSeqGPMMkits, s"mesh$name/gtTargetMapAligned.vtk")

        val target = MeshIO.readScalarMeshField[Int](gtmap).get
        val reconstruction = MeshIO.readScalarMeshField[Int](reconstructionmap).get
        val labelMapOnGTshape = MeshIO.readScalarMeshField[Int](labelMapOnGTshapemap).get

        val result = SegmentationSummary(labelMapOnGTshape, target)

        println(name)

        val tumorGTMesh = target.mesh.operations.clip(p => target.pointsWithValues.find(_._1 == p).get._2 == 0)
        val healthyGTMesh = target.mesh.operations.clip(p => target.pointsWithValues.find(_._1 == p).get._2 == 1)
        val tumorReconstructionMesh = labelMapOnGTshape.mesh.operations.clip(p => labelMapOnGTshape.pointsWithValues.find(_._1 == p).get._2 == 0)
        val healthyReconstructionMesh = reconstruction.mesh.operations.clip(p => reconstruction.pointsWithValues.find(_._1 == p).get._2 == 1)

        val diceScore_tumor: Double = MeshMetrics.diceCoefficient(tumorReconstructionMesh, tumorGTMesh)
        val diceScore_healthy: Double = MeshMetrics.diceCoefficient(healthyReconstructionMesh, healthyGTMesh)
        println(s"dice score: tumor $diceScore_tumor, healthy $diceScore_healthy")

        avgTumor = avgTumor ++ Seq(diceScore_tumor)
        avgKidney = avgKidney ++ Seq(diceScore_healthy)
        avgF1 = avgF1 ++ Seq(result.f1Score)
        avgMCC = avgMCC ++ Seq(result.MCC)

      }

    println("avg tumor dice", avgTumor.sum / avgTumor.length)
    println("avg kidney dice", avgKidney.sum / avgKidney.length)
    println("avg tumor kidney dice", 0.5 * (avgTumor.sum / avgTumor.length + avgKidney.sum / avgKidney.length))
    println("avg f1", avgF1.sum / avgF1.length)
    println("avg mcc", avgMCC.sum / avgMCC.length)
    println("length", avgMCC.length)
  }

}
