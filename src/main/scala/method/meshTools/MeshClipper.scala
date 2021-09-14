package method.meshTools

import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.PointId
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.{TriangleMesh, TriangleMesh3D}

object MeshClipper {

  def usingScalarMeshField(mesh: TriangleMesh3D, referenceMeshField: ScalarMeshField[Short]): TriangleMesh3D = {
    val keepIds: IndexedSeq[PointId] = referenceMeshField.pointsWithValues
      .filter { set =>
        set._2 == 0.toShort
      }
      .map {
        case (p, v) => referenceMeshField.mesh.pointSet.pointId(p).get
      }
      .toIndexedSeq
    println("number of points to keep = " + keepIds.length.toString + " from total = " + referenceMeshField.mesh.pointSet.points.length)
    println("0 < number of points to keep < number of points on initial mesh? " + (0 < keepIds.length && keepIds.length < mesh.pointSet.points.length).toString)
    println("clipping reference mesh...")
    val meshClipped: TriangleMesh[_3D] = mesh.operations.clip { p =>
      val id = mesh.pointSet.pointId(p).get
      keepIds.contains(id) == false
    }
    println("clipping done, clipped mesh number of points = " + meshClipped.pointSet.points.length)
    meshClipped
  }

  def usingPointIds(mesh: TriangleMesh3D, idsToKeep: IndexedSeq[PointId]): TriangleMesh3D = {
    val ptsToRemove = idsToKeep.map { id =>
      mesh.pointSet.point(id)
    }.toIndexedSeq
    mesh.operations.clip { pt =>
      ptsToRemove.contains(pt)
    }
  }

  def usingSphere(mesh: TriangleMesh3D, center: Point[_3D], nPointsToRemove: Int): TriangleMesh3D = {
    val ptsToRemove = mesh.pointSet
      .findNClosestPoints(center, nPointsToRemove)
      .map {
        _.point
      }
      .toIndexedSeq
    mesh.operations.clip { pt =>
      ptsToRemove.contains(pt)
    }
  }

  def usingSphereWithIds(mesh: TriangleMesh3D, center: Point[_3D], nPointsToRemove: Int): (TriangleMesh3D, IndexedSeq[PointId]) = {
    val ptsToRemove = mesh.pointSet
      .findNClosestPoints(center, nPointsToRemove)
      .map {
        _.point
      }
      .toIndexedSeq
    val idsToKeep = mesh.pointSet.pointIds.toIndexedSeq.filterNot { pid =>
      ptsToRemove.contains(mesh.pointSet.point(pid))
    }
    (mesh.operations.clip { pt =>
      ptsToRemove.contains(pt)
    }, idsToKeep)
  }

}
