package method.meshTools

import scalismo.geometry.{EuclideanVector3D, Point, _3D}
import scalismo.mesh.TriangleMesh3D

object CorrespondenceSearch {

  def getIntersectionPointsWithSimilarNormal(
      target: TriangleMesh3D,
      point: Point[_3D],
      normal: EuclideanVector3D
  ): IndexedSeq[Point[_3D]] = {
    val maxDistanceThreshold = 10
    val directionThreshold = 1 - 0.1
    val intersectionPoints = target.operations.getIntersectionPoints(point, normal)
    val candidates = intersectionPoints /*.zip(intersectionNormals)*/ .filter { p =>
      val closeEnough = (p - point).norm < maxDistanceThreshold
      // NOTE: The next line approximates the normal of the intersection point with the normal of its closest vertex
      val nTarget = target.vertexNormals(target.pointSet.findClosestPoint(p).id)
      val similarNormal = normal.normalize.dot(nTarget.normalize) > directionThreshold
      similarNormal && closeEnough
    }.toIndexedSeq // NOTE: choose one of the candidates or fall back to 10 nearest neighbors
    candidates
  }
}
