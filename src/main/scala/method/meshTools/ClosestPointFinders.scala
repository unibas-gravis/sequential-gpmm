package method.meshTools

import scalismo.geometry
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.mesh.TriangleMesh

// TODO: could these methods go to the class CorrespondenceSearch???
object ClosestPointFinders {

  def getClosestPointIncludingNormal(meshToProjectTo: TriangleMesh[_3D],
                                     point: Point[_3D],
                                     normal: EuclideanVector[_3D]): (Point[_3D], geometry.EuclideanVector[_3D]) = {
    val closestPointAndId = meshToProjectTo.pointSet.findNClosestPoints(point, 10)
    val ptWithNormal = closestPointAndId.map { ptWithID =>
      (ptWithID.point, meshToProjectTo.vertexNormals.atPoint(ptWithID.id))
    }
    val (bestPoint, bestNormal) = ptWithNormal.maxBy {
      case (_, normalOnTarget) =>
        val normProd = normalOnTarget.norm //*normal.norm commented because always = 1
        val dotProd = normalOnTarget.dot(normal)
        dotProd / normProd
    }
    (bestPoint, bestNormal)
  }

  def getClosestPointAlongIntersection(target: TriangleMesh[_3D],
                                       queryPoint: Point[_3D],
                                       queryNormal: EuclideanVector[_3D]): (Point[_3D], EuclideanVector[_3D]) = {
    val candidates = CorrespondenceSearch.getIntersectionPointsWithSimilarNormal(target, queryPoint, queryNormal)
    if (candidates.length == 0) {
      val set = target.pointSet
        .findClosestPoint(queryPoint)
      val p = set.point
      val id = set.id
      val n = target.vertexNormals.atPoint(id)
      (p, n)
    } else {
      val p = candidates.head
      val id = target.pointSet.findClosestPoint(p).id
      val n = target.vertexNormals.atPoint(id)
      (p, n)
    }
  }

}
