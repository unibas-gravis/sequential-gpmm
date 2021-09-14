package application.syntheticDataset

import java.io.File

import application.syntheticDataset.SeqGpmmFitting.{loadModel, loadTarget}
import method.fittingTools.TemplateRegistrationEvaluationHelpers
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.ScalarMeshField
import scalismo.io.MeshIO
import scalismo.ui.api.ScalismoUI

object IcpLabelMapGeneration {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val resultDir = SyntheticData.outputDirNRICP
    val (model, _) = loadModel()

    SyntheticData.allNames.sorted.zipWithIndex.foreach {
      case (name, index) =>
        println(s"processing ${name} ...")
        val fit = MeshIO.readMesh(new File(resultDir, s"mesh$name/fit.vtk")).get
        val (target, _) = loadTarget(name)
        val ui = ScalismoUI()
        ui.show(fit, "fit")
        ui.show(target, "target")
        val w = name match {
          case name if name.contains("102") => 0.05
          case name if name.contains("205") => 0.1
          case name if name.contains("411") => 0.2
          case name if name.contains("686") => 0.33
          case _ => 0.0
        }
        //w is the expected fraction of outliers.
        // The synthetic dataset was created by clipping out 5%, 10%, 20%, or 33% of the target mesh.
        // Therefore for each target mesh, we set w to be the known fraction of outliers.

        val data = TemplateRegistrationEvaluationHelpers.icp.getIcpLabelMapData(fit, target).toArray.toIndexedSeq
        val labels = data.map{v=>
          if (v==1) 0 else 1
        }
        val labelMap: ScalarMeshField[Int] = ScalarMeshField[Int](model.reference, labels)
        MeshIO.writeScalarMeshField[Int](labelMap, new File(resultDir, s"mesh$name/labelMapOnRef.vtk"))
        MeshIO.writeMesh(fit, new File(resultDir, s"mesh$name/fit.vtk"))
        ui.show(labelMap, "labelmap")
    }


  }

}
