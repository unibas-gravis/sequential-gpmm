package application.syntheticDataset

import java.io.File

import method.fittingTools.SequentialGpmmFitter
import method.meshTools
import method.meshTools.ClosestPointFinders
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.geometry.{Landmark, _3D}
import scalismo.io.{LandmarkIO, MeshIO, StatisticalModelIO}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.{PointDistributionModel, StatisticalMeshModel}
import scalismo.ui.api.ScalismoUI

object SeqGpmmFitting {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val ui = ScalismoUI()

    val CI = 0.3

    val (ssm, modelLandmarks) = loadModelWithFilteredLandmarks()

    SyntheticData.allNames.sorted.foreach { name =>

      println(s"processing ${name} ...")

      val (target, targetLandmarks) = loadTargetWithFilteredLandmarks(name)

      val alignment = meshTools.Alignment.getTransformation(targetLandmarks, modelLandmarks, ssm)
      val alignedTarget = target.transform(alignment)

      val mainOutputFolder = new File(SyntheticData.outputDirSeqGPMM, s"mesh$name")
      if (!mainOutputFolder.exists()) mainOutputFolder.mkdirs()

      val labelMapOnFit: ScalarMeshField[Int] =
        SequentialGpmmFitter(alignedTarget, ssm, mainOutputFolder, correspondenceFunction = ClosestPointFinders.getClosestPointIncludingNormal)
          .runSequentialGP(modelLandmarks, CI)

      ui.show(labelMapOnFit, s"labelmaponfit$name")
      ui.show(alignedTarget, s"target$name")
    }

  }

  private def landmarkFilter(lm: Landmark[_3D]): Boolean = {
    !(lm.id.contains("isdom") || lm.id.contains("cranium"))
  }

  def loadModel(): (PointDistributionModel[_3D, TriangleMesh], IndexedSeq[Landmark[_3D]]) = {
    val allLMS = LandmarkIO.readLandmarksJson[_3D](SyntheticData.reflmFile).get.toIndexedSeq
    val innerLMS = LandmarkIO.readLandmarksJson[_3D](SyntheticData.innerreflmFile).get.toIndexedSeq
    val modelLMS = (allLMS ++ innerLMS)
    val augmentedModel = StatisticalModelIO.readStatisticalTriangleMeshModel3D(SyntheticData.modelFile).get
    (augmentedModel, modelLMS)
  }

  def loadModelWithFilteredLandmarks(): (PointDistributionModel[_3D, TriangleMesh], IndexedSeq[Landmark[_3D]]) = {
    val (ssm, modelLandmarks) = loadModel()
    val modelLandmarksFiltered = modelLandmarks.filter(landmarkFilter)
    (ssm, modelLandmarksFiltered)
  }

  def loadTarget(name: String): (TriangleMesh[_3D], Seq[Landmark[_3D]]) = {
    val target = MeshIO.readMesh(new File(SyntheticData.inputLocation, s"$name.stl")).get
    val targetLandmarkFile = SyntheticData.landmarkLocation.listFiles().filter(_.getName.contains(name)).head
    val targetLandmarks = LandmarkIO.readLandmarksJson[_3D](targetLandmarkFile).get
    (target, targetLandmarks)
  }

  def loadTargetWithFilteredLandmarks(name: String): (TriangleMesh[_3D], Seq[Landmark[_3D]]) = {
    val (target, targetLandmarks) = loadTarget(name)
    val targetLandmarksFiltered = targetLandmarks.filter(landmarkFilter).toIndexedSeq
    (target, targetLandmarksFiltered)
  }

}
