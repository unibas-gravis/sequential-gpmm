package method.meshTools

import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.utils.MeshConversion

object Remeshing {

  def decimate(mesh: TriangleMesh[_3D], targetedNumberOfVertices: Int): TriangleMesh[_3D] = {
    val refVtk = MeshConversion.meshToVtkPolyData(mesh)
    val decimate = new vtk.vtkQuadricDecimation()
    val reductionRate = 1.0 - (targetedNumberOfVertices / mesh.pointSet.numberOfPoints.toDouble)
    decimate.SetTargetReduction(reductionRate)
    decimate.SetInputData(refVtk)
    decimate.Update()
    val decimatedRefVTK = decimate.GetOutput()
    MeshConversion.vtkPolyDataToTriangleMesh(decimatedRefVTK).get
  } //decrease n vertices

  def subdivide(mesh: TriangleMesh[_3D]): TriangleMesh[_3D] = {
    val refVtk = MeshConversion.meshToVtkPolyData(mesh)
    val subdivide = new vtk.vtkLinearSubdivisionFilter()

    subdivide.SetNumberOfSubdivisions(1)

    subdivide.SetInputData(refVtk)
    subdivide.Update()
    val subdividedRefVTK = subdivide.GetOutput()
    MeshConversion.vtkPolyDataToTriangleMesh(subdividedRefVTK).get
  } //increase n vertices

}
