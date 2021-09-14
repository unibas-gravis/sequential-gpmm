package method.meshTools

import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.{PointId, ScalarMeshField}
import scalismo.geometry.{Landmark, Point, _3D}
import scalismo.mesh.TriangleMesh3D

object MeshLabeler {
  // in ScalismoUI, labels foreground points blue on mesh, keeps all other points on mesh red
  // foreground = 0 = blue (inliers), background = 1 = red (outliers)
  def labelForegroundPoints(mesh: TriangleMesh3D, foregroundPoints: IndexedSeq[Point[_3D]]): ScalarMeshField[Int] = {

    val foregroundIds = foregroundPoints.map { p =>
      mesh.pointSet.findClosestPoint(p).id
    }
    val data = mesh.pointSet.pointsWithId.map {
      case (p, id) =>
        if (foregroundIds.contains(id)) 0 else 1
    }.toIndexedSeq
    ScalarMeshField(mesh, data)
  }

  def labelForegroundPointsFromIDs(mesh: TriangleMesh3D, foregroundIds: IndexedSeq[PointId]): ScalarMeshField[Int] = {
    val data = mesh.pointSet.pointIds.map { id =>
      if (foregroundIds.contains(id)) 0 else 1
    }.toIndexedSeq
    ScalarMeshField(mesh, data)
  }
}
