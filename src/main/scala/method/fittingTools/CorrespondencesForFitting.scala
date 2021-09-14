package method.fittingTools

import method.meshTools.ClosestPointFinders
import scalismo.common.PointId
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh

// TODO: could this method go to the class CorrespondenceSearch???
object CorrespondencesForFitting {

  def getCorrespondences(
      sample: TriangleMesh[_3D],
      target: TriangleMesh[_3D]
  ): ((IndexedSeq[Point[_3D]], IndexedSeq[PointId]), (IndexedSeq[Point[_3D]], IndexedSeq[PointId])) = {
    val pointsOnSample = sample.pointSet.points.toIndexedSeq
    val pointsLabeledAsForeground = pointsOnSample.map(p => target.operations.closestPointOnSurface(p)).map(_.point)
    val pointsCurrentMesh = pointsLabeledAsForeground.map(p => sample.operations.closestPointOnSurface(p)).map(_.point)
    val pointIdsOnCurrentMesh = pointsCurrentMesh.map(sample.pointSet.findClosestPoint(_).id)
    val pairs = (pointsLabeledAsForeground, pointIdsOnCurrentMesh)
    val idsBG = sample.pointSet.pointIds.filterNot {
      pairs._2.contains(_)
    }.toIndexedSeq
    val closestPointsToBG = idsBG.map { id =>
      val pt = sample.pointSet.point(id)
      target.operations.closestPointOnSurface(pt).point
    }
    val pairsBG = (closestPointsToBG, idsBG)
    (pairs, pairsBG)
  }

}
