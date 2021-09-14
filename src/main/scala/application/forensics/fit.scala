package application.forensics

import java.io.File

import application.syntheticDataset.SyntheticData
import method.fittingTools.SequentialGpmmFitter
import method.meshTools
import scalismo.common.ScalarMeshField
import scalismo.geometry._3D
import scalismo.io.{LandmarkIO, MeshIO, StatismoIO, StatisticalModelIO}
import scalismo.mesh.TriangleMesh
import scalismo.ui.api.ScalismoUI

object fit {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val allLMS = LandmarkIO.readLandmarksJson[_3D](SyntheticData.reflmFile).get.toIndexedSeq

    val innerLMS = LandmarkIO.readLandmarksJson[_3D](SyntheticData.innerreflmFile).get.toIndexedSeq

    val modelLMS = (allLMS ++ innerLMS)
      .filterNot { lm =>
        lm.id.contains("isdom") || lm.id.contains("cranium")
      }
      .sortBy(_.id).take(3)

    val ui = ScalismoUI()

    val file = ForensicsData.targetFile
    val target: TriangleMesh[_3D] = MeshIO.readMesh(file).get
    val name = file.getName.replace(".vtk", "")

    val augmentedModel = StatisticalModelIO.readStatisticalTriangleMeshModel3D(SyntheticData.modelFile).get

    val CI = 0.3

    val mainOutputFolder = new File(ForensicsData.forensicsDir, "outputSeqGPMM")
    mainOutputFolder.mkdirs()

    val labelMapOnFit = SequentialGpmmFitter(target, augmentedModel, mainOutputFolder).runSequentialGP(modelLMS, CI)
    ui.show(labelMapOnFit, s"labelmaponfit$name")

    val teethLabelMapFloat = MeshIO.readScalarMeshField[Float](ForensicsData.labelMapTeethFile).get
    val teethLabelMap = ScalarMeshField[Int](teethLabelMapFloat.mesh, teethLabelMapFloat.data.toIndexedSeq.map { _.toInt })

    val cleanedLabels = labelMapOnFit.pointsWithValues.map {
      case (pt, value) =>
        val id = labelMapOnFit.mesh.pointSet.pointId(pt).get
        if (value == 0) { 0 } else {
          val onTeethLabelMap = teethLabelMap.mesh.pointSet.point(id)
          val label = teethLabelMap.pointsWithValues.find(_._1 == onTeethLabelMap).get._2
          if (label == 1) { 1 } else { 0 }
        }
    }.toIndexedSeq
    val labelMapOnFitCleaned = ScalarMeshField[Int](labelMapOnFit.mesh, cleanedLabels)

    ui.show(labelMapOnFitCleaned, "clean")
    ui.show(target,"target")
    MeshIO.writeScalarMeshField[Int](labelMapOnFitCleaned, new File(mainOutputFolder, "labelMapOnFitCleaned.vtk"))

  }

}
