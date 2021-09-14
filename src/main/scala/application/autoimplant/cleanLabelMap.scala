package application.autoimplant

import java.io.File

import scalismo.io.MeshIO
import scalismo.ui.api.ScalismoUI
import scalismo.common.ScalarMeshField

object cleanLabelMap {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val ui = ScalismoUI()

    val name = "001"
    val mainOutputFolder = new File(AutoImplantData.outputDirSeqGPMMautoimplant, s"mesh$name")

    val fittedLabelMap = MeshIO.readScalarMeshField[Int](new File(mainOutputFolder, "labelMapOnFit.vtk")).get
    val fittedDmahalMap = MeshIO.readScalarMeshField[Double](new File(mainOutputFolder, "dmahalsquaredMapOnFit.vtk")).get
    val refLabelMap = MeshIO.readScalarMeshField[Int](AutoImplantData.refLabelMap).get

    ui.show(fittedLabelMap, s"fittedLabelMap-$name")
    ui.show(refLabelMap, s"refLabelMap-$name")
    ui.show(fittedDmahalMap, s"fittedDmahalMap-$name")
    val target = MeshIO.readMesh(new File(mainOutputFolder, "target.vtk")).get
    val fit = MeshIO.readMesh(new File(mainOutputFolder, "fit.vtk")).get
    val targetFull = MeshIO.readMesh(AutoImplantData.gtalignedDir.listFiles().filter(_.getName.contains(name)).head).get
    ui.show(target, s"target-$name")
    ui.show(fit, s"fit-$name")

    val implantGTdata = targetFull.pointSet.points.map { p =>
      if (target.pointSet.points.contains(p)) 0 else 1
    }.toIndexedSeq
    val implantGTLabelMap = ScalarMeshField[Int](targetFull, implantGTdata)
    ui.show(implantGTLabelMap, s"implantGTLabelMap")
    val implantGTPoints = implantGTLabelMap.pointsWithValues.filter(_._2 == 1).map(_._1).toIndexedSeq
    val implantGT = implantGTLabelMap.mesh.operations.clip(pt => implantGTPoints.contains(pt) == false)
    ui.show(implantGT, s"implantGT-$name")

    val cleanedDir = new File(mainOutputFolder, "cleaned")
    cleanedDir.mkdirs()

    val cleanedData = refLabelMap.data.zip(fittedLabelMap.data).map {
      case (labelRef, labelFit) =>
        (labelRef, labelFit) match {
          case (0, 1) => 0
          case _      => labelFit
        }
    }
    val cleanedFittedLabelMap = ScalarMeshField[Int](fittedLabelMap.mesh, cleanedData)
    ui.show(cleanedFittedLabelMap, s"cleanedFittedLabelMap-$name")

    MeshIO.writeScalarMeshField[Int](cleanedFittedLabelMap, new File(cleanedDir, "cleanedLabelMapOnFit.vtk"))
    MeshIO.writeScalarMeshField[Int](implantGTLabelMap, new File(cleanedDir, "implantGTLabelMap.vtk"))
    MeshIO.writeMesh(implantGT, new File(cleanedDir, s"implantGT.stl"))

  }

}
