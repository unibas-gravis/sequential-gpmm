package application.autoimplant

import java.io.File

import method.fittingTools.SequentialGpmmFitter
import method.meshTools
import scalismo.geometry._
import scalismo.io.{LandmarkIO, MeshIO, StatisticalModelIO}
import scalismo.ui.api.ScalismoUI

object fit {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val modelLMSouter = LandmarkIO
      .readLandmarksJson[_3D](AutoImplantData.modelLmsDir)
      .get
      .toIndexedSeq
      .filterNot(_.id.contains(".in."))

    val extraLmsRef = LandmarkIO
      .readLandmarksJson[_3D](
        AutoImplantData.modelLmsDirExtra
      )
      .get
      .toIndexedSeq
    val modelLMSinit = modelLMSouter ++ extraLmsRef

    val augmentedModel = StatisticalModelIO.readStatisticalTriangleMeshModel3D(AutoImplantData.modelDir).get

    AutoImplantData.targetDir.listFiles.foreach { file =>
      val name = file.getName.replace(".vtk", "").replace(".stl","")
      println(name)

      val targetInit = MeshIO.readMesh(file).get
      println(s"targetInit ${targetInit.pointSet.numberOfPoints}")

      val lmsTargetFile = AutoImplantData.targetLandmarksFile.listFiles().filter(_.getName.contains(name)).head
      val targetLMSforAlignment = LandmarkIO
        .readLandmarksJson[_3D](lmsTargetFile)
        .get
        .toIndexedSeq
        .filter(
          l =>
            l.id.contains("cranium.suture")
              || l.id.contains("cranium.back")
              || l.id.contains("cranium.bregma")
              || l.id.contains("cranium.nose.center"))

      val modelLMSforAlignment = modelLMSinit.filter { lm =>
        targetLMSforAlignment.map { _.id }.contains(lm.id)
      }

      val transformation = meshTools.Alignment.getTransformation(targetLMSforAlignment, modelLMSforAlignment, augmentedModel)
      val targetAligned = targetInit.transform(transformation)

      val modelLMSforInit = modelLMSinit.filter(
        l =>
          l.id.contains("cranium.inner")
            || l.id.contains("cranium.temporal")
            || l.id.contains("cranium.suture")
            || l.id.contains("cranium.back")
            || l.id.contains("cranium.bregma")
            || l.id.contains("cranium.nose.center"))

      val CI = 0.3
      val mainOutputFolder = new File(AutoImplantData.outputDirSeqGPMMautoimplant, s"mesh$name")
      mainOutputFolder.mkdirs()

      val ui = ScalismoUI()
      ui.show(ui.createGroup("model"), augmentedModel, s"augmentedModel")
      ui.show(targetAligned, "target")
      val labelMapOnFit = SequentialGpmmFitter(targetAligned, augmentedModel, mainOutputFolder).runSequentialGP(modelLMSforInit, CI)
      ui.show(labelMapOnFit, s"labelmaponfit$name")

    }

  }

}
