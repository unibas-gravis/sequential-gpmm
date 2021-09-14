package application.syntheticDataset

import java.io.File

import application.syntheticDataset.SeqGpmmFitting.{loadModel, loadTarget}
import method.fittingTools.TemplateRegistrationEvaluationHelpers
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.{DiscreteField, ScalarMeshField}
import scalismo.io.MeshIO
import scalismo.mesh.Mesh
import scalismo.ui.api.ScalismoUI

object CpdLabelMapGeneration {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val resultDir = SyntheticData.outputDirCPD
    val (model, _) = loadModel()
    val sigma2s = Seq(0.1808333289382017,0.16988748755138575,0.16479288071680687,0.16966132953033372,0.1476155310641469,
      0.17583044305857337,0.1816727944680331,0.1722096891659723,0.18554006391798625,0.18500554162058838,
      0.2881656980375178,0.19029474138887453,0.21629954558343897,0.21826922159258486,0.25838386147570447,
      0.16050075138794645,0.152669196366081,0.16446624304273907,0.16926960370220326,0.16772249622210028,
      0.46906590080306787, 0.2045781597025774, 0.3596066059852045, 0.2089566155660731, 0.21655894624549207,
      0.20271747676937052, 0.316254452918899, 0.21496484829487, 0.1779313375376926, 0.2487369542995678,
      1.6301678366485428, 0.40777816639222236, 1.677407741829371, 0.8002602085982063 ,0.42102370413379164,
      1.7951824060259851, 0.9970550745983515, 0.53977185187201, 0.43183257324503577, 0.43115275772724887)
    // The sigma2s are the remaining variance for each mesh, given as output by the CPD registration method.
    // Please refer to "Implementation of other fitting methods" in the README for more information.

    SyntheticData.allNames.sorted.zipWithIndex.foreach {
      case (name, index) =>
        println(s"processing ${name} ...")
        val fit = MeshIO.readMesh(new File(resultDir, s"mesh$name/fit.vtk")).get
        val (target, _) = loadTarget(name)
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

        val sigma2 = sigma2s(index)
        val data = TemplateRegistrationEvaluationHelpers.cpd.getCorrespondence(fit.pointSet.points.toSeq, target.pointSet.points.toSeq, w, sigma2).toArray.toIndexedSeq
        val threshold = 0.001
        val labels = data.map { v =>
          if (v < threshold) 1 else 0
        }
        val labelMap: ScalarMeshField[Int] = ScalarMeshField[Int](model.reference, labels)
        val labelMapfit: ScalarMeshField[Int] = ScalarMeshField[Int](fit, labels)
        MeshIO.writeScalarMeshField[Int](labelMap, new File(resultDir, s"mesh$name/labelMapOnRef.vtk"))
        MeshIO.writeScalarMeshField[Int](labelMapfit, new File(resultDir, s"mesh$name/labelMapOnFit.vtk"))

    }


  }

}
