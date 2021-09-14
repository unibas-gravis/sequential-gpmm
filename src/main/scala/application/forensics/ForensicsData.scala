package application.forensics

import java.io.File

import application.DataDirectory
import scalismo.geometry.{Landmark, _3D}
import scalismo.io.{LandmarkIO, StatismoIO}
import scalismo.statisticalmodel.StatisticalMeshModel

object ForensicsData {

  val forensicsDir = new File(DataDirectory.dir, "forensics")

  val targetFile: File = new File(forensicsDir, "target.vtk")

  val labelMapTeethFile: File = new File(forensicsDir, "labelMapTeethOnRef.vtk")
}
