package application.KITS

import java.io.File

import application.DataDirectory
import method.fittingTools.SequentialGpmmFitter
import method.meshTools
import method.meshTools.ClosestPointFinders
import scalismo.common.ScalarMeshField
import scalismo.geometry.{Landmark, _3D}
import scalismo.io.{LandmarkIO, MeshIO, StatismoIO, StatisticalModelIO}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.StatisticalMeshModel
import scalismo.statisticalmodel.dataset.DataCollection
import scalismo.ui.api.ScalismoUI
import scalismo.utils.Random.implicits._

object fit {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val lmstargetfiles = KitsData.targetlmFile.listFiles()
    val lmsModel = LandmarkIO.readLandmarksJson[_3D](KitsData.landmarksOnModelReferenceFile).get
    val model = StatisticalModelIO.readStatisticalTriangleMeshModel3D(KitsData.modelFile).get
    val targetFiles = KitsData.targetFile.listFiles()

    val CI = 0.5
    val ui = ScalismoUI()
    ui.show(ui.createGroup("model"), model, "model")
    targetFiles.sortBy(_.getName).foreach { file =>
      val targetInit: TriangleMesh[_3D] = MeshIO.readMesh(file).get
      val name = file.getName.replace(".vtk", "")
      println(name)
      println(s"targetInit ${targetInit.pointSet.numberOfPoints}")

      val lmsTargetFile = lmstargetfiles.filter(_.getName.contains(name)).head
      val lmsTarget = LandmarkIO
        .readLandmarksJson[_3D](lmsTargetFile)
        .get
        .filter(_.id.contains("left"))
        .filterNot(_.id.contains("out"))
      println(lmsTarget.length)

      val lmsModelFilteredInit = lmsTarget.map { lm =>
        lmsModel.find(_.id == lm.id).head
      }.toIndexedSeq

      val lmsModelFiltered = lmsModelFilteredInit.map { lm =>
        Landmark[_3D](lm.id, model.mean.pointSet.findClosestPoint(lm.point).point)
      }

      val lmsModelFilteredRef = lmsModelFilteredInit.map { lm =>
        Landmark[_3D](lm.id, model.reference.pointSet.findClosestPoint(lm.point).point)
      }

      val mainOutputFolder = new File(KitsData.outputDirSeqGPMMkits, s"mesh$name")
      mainOutputFolder.mkdirs()

      val transformation = meshTools.Alignment.getTransformation(lmsTarget, lmsModelFiltered, model)
      val targetAligned = targetInit.transform(transformation)
      ui.show(ui.createGroup("targetaligned"), targetAligned, "targetAligned")

      val lmsForSeqFit = lmsModelFilteredRef//.filter{lm=>lm.id.contains("front")||lm.id.contains("back")}
      lmsForSeqFit.foreach(lm=>println(lm.id))
      val labelMapOnFit =
        SequentialGpmmFitter(targetInit, model, mainOutputFolder)
          .runSequentialGP(lmsForSeqFit, CI)

      ui.show(labelMapOnFit, s"labelmaponfit$name")

      val gtmap = new File(KitsData.gtmaplocation, s"$name.vtk")
      val gtTargetMap = MeshIO.readScalarMeshField[Int](gtmap).get
      val gtTargetMapAligned = ScalarMeshField(targetAligned, gtTargetMap.data)
      ui.show(gtTargetMapAligned, "gtTargetMapAligned")
      MeshIO.writeScalarMeshField[Int](gtTargetMapAligned, new File(mainOutputFolder, "gtTargetMapAligned.vtk"))

      val labelMapOnGTshapeData = targetAligned.pointSet.pointsWithId.map {
        case (pt, _) =>
          val pOnFit = labelMapOnFit.mesh.pointSet.findClosestPoint(pt).point
          val labelOnFIt = labelMapOnFit.pointsWithValues.filter(_._1 == pOnFit).toIndexedSeq.head._2
          labelOnFIt
      }
      val labelMapOnGTshape = ScalarMeshField[Int](targetAligned, labelMapOnGTshapeData.toIndexedSeq)
      ui.show(labelMapOnGTshape, "labelMapOnGTshape")
      MeshIO.writeScalarMeshField[Int](labelMapOnGTshape, new File(mainOutputFolder, "labelMapOnGTshape.vtk"))
      MeshIO.writeScalarMeshField[Int](gtTargetMapAligned, new File(mainOutputFolder, "gtTargetMapAligned.vtk"))
      MeshIO.writeScalarMeshField[Int](labelMapOnFit, new File(mainOutputFolder, "labelMapOnFit.vtk"))



    }
  }

}
