package application.autoimplant

import breeze.numerics.sqrt
import scalismo.common.BoxDomain
import scalismo.geometry._3D
import scalismo.io.MeshIO
import scalismo.mesh.TriangleMesh
import scalismo.numerics.UniformSampler
import scalismo.ui.api.ScalismoUI
import scalismo.utils.Random.implicits._

import java.io.File

object evaluate {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val resultFilesLoc = AutoImplantData.outputDirSeqGPMMautoimplant

    val resultFiles = resultFilesLoc.listFiles().toIndexedSeq

    val ui = ScalismoUI()

    def labeledDiceCoefficient(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D], evaluationRegion: BoxDomain[_3D]): Double = {
      val imgA = m1.operations.toBinaryImage
      val imgB = m2.operations.toBinaryImage

      val sampler = UniformSampler[_3D](evaluationRegion, 10000)
      val samplePts = sampler.sample().map(_._1)

      val numSamplesInA = samplePts.map(imgA).sum
      val numSamplesInB = samplePts.map(imgB).sum
      val AIntersectB = (imgA + imgB) andThen ((v: Short) => if (v > 1 + 1e-5) 1.0f else 0.0f)

      val numSamplesInAIB = samplePts.map(AIntersectB).sum
      2 * numSamplesInAIB / (numSamplesInA + numSamplesInB)
    }

    val results = resultFiles.sorted.map { file =>
      val name = file.getName.replace("mesh", "")
      println(name)
      val fittedMesh = MeshIO.readMesh(new File(file, "fit.vtk")).get
      val cleanedDir = new File(file, "cleaned")
      val implantGTlabelMap = MeshIO.readScalarMeshField[Int](new File(cleanedDir, "implantGTLabelMap.vtk")).get
      val implantGT = MeshIO.readMesh(new File(cleanedDir, "implantGT.stl")).get
      ui.show(implantGTlabelMap, "implantGTlabelMap")
      ui.show(fittedMesh, "fittedMesh")

      val distancesImplant = implantGTlabelMap.pointsWithValues
        .filter(_._2 == 1)
        .map {
          case (p, v) =>
            sqrt(
              fittedMesh.operations
                .closestPointOnSurface(p)
                .distanceSquared)
        }
        .toIndexedSeq

      val hdpatho = distancesImplant.max
      println(s"hdpatho $hdpatho")
      val adpatho = distancesImplant.sum / distancesImplant.length
      println(s"adpatho $adpatho")

      val dicescorepatho = labeledDiceCoefficient(fittedMesh, implantGTlabelMap.mesh, implantGT.pointSet.boundingBox)
      println(s"dicescorepatho $dicescorepatho")

      (dicescorepatho, adpatho, hdpatho)
    }

    val dicescorepathoavg = results.map(_._1).sum / results.length
    val adpathoavg = results.map(_._2).sum / results.length
    val hdpathoavg = results.map(_._3).sum / results.length
    println(dicescorepathoavg, adpathoavg, hdpathoavg)

  }
}
