package application.syntheticDataset

import java.io.File

import method.fittingTools.RansacFitter
import method.meshTools
import scalismo.geometry._3D
import scalismo.io.{LandmarkIO, MeshIO, StatisticalModelIO}
import scalismo.ui.api.ScalismoUI

object RansacFitting {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()
    val ui = ScalismoUI()

    val allLMS = LandmarkIO.readLandmarksJson[_3D](SyntheticData.reflmFile).get.toIndexedSeq
    val innerLMS = LandmarkIO.readLandmarksJson[_3D](SyntheticData.innerreflmFile).get.toIndexedSeq
    val modelLMS = (allLMS ++ innerLMS).filterNot { lm =>
      lm.id.contains("isdom") || lm.id.contains("cranium")
    }
    val augmentedModel = StatisticalModelIO.readStatisticalTriangleMeshModel3D(SyntheticData.modelFile).get
    val lmstargetfiles = SyntheticData.landmarkLocation.listFiles()
    val lmsTargetFile = lmstargetfiles.filter(_.getName.contains("686-1")).head //todo replace 686-1 with name, and add inner condyle lms to all target lm files

    SyntheticData.allNames.sorted.foreach { name =>
      println(name)

      val initTarget = MeshIO.readMesh(new File(SyntheticData.inputLocation, s"$name.stl")).get

      val lmsTarget = LandmarkIO
        .readLandmarksJson[_3D](lmsTargetFile)
        .get
        .filterNot { lm =>
          lm.id.contains("isdom") || lm.id.contains("cranium")
        }
        .toIndexedSeq
      val lmsModelFiltered = modelLMS
      val transformation = meshTools.Alignment.getTransformation(lmsTarget, lmsModelFiltered, augmentedModel)
      val target = initTarget.transform(transformation)

      val mainOutputFolder = new File(SyntheticData.outputDirRANSAC, s"mesh$name")
      mainOutputFolder.mkdirs()

      val labelmap = RansacFitter.fitWithRansac(target, augmentedModel, mainOutputFolder, 0.3, 0.6)
      ui.show(labelmap, s"labelmap$name").opacity_=(0.0)
      ui.show(target, s"target$name").opacity_=(0.0)

    }
  }
}
