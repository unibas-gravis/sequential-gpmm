package method.meshTools

import scalismo.geometry.{Landmark, Point, Point3D, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.PointDistributionModel
import scalismo.transformations.RigidTransformation

object Alignment {

  def getTransformation(targetLms: Seq[Landmark[_3D]],
                        modelLms: Seq[Landmark[_3D]],
                        model: PointDistributionModel[_3D, TriangleMesh]): RigidTransformation[_3D] = {
    val landmarkPairs = targetLms.map { lm =>
      val name = lm.id
      val correspondingLm = modelLms.find(_.id == name).get
      (lm.point, correspondingLm.point)
    }
    val n = model.mean.pointSet.numberOfPoints
    val center = (model.mean.pointSet.points.map(_.toVector).reduceLeft(_ + _) / n).toPoint
    LandmarkRegistration.rigid3DLandmarkRegistration(landmarkPairs, center)
  }

  def commonLandmarkPairs(targetLms: Seq[Landmark[_3D]], modelLms: Seq[Landmark[_3D]]): Seq[(Point[_3D], Point[_3D])] = {
    val commonLmNames = targetLms.map(_.id) intersect modelLms.map(_.id)

    commonLmNames.map(name => (targetLms.find(_.id == name).get.point, modelLms.find(_.id == name).get.point))
  }

  def getTransformationToOrigin(targetLms: Seq[Landmark[_3D]], modelLms: Seq[Landmark[_3D]]): RigidTransformation[_3D] = {

    val landmarkPairs = commonLandmarkPairs(targetLms, modelLms)

    val centerpoint = Point3D(0.0, 0.0, 0.0)

    LandmarkRegistration.rigid3DLandmarkRegistration(landmarkPairs, centerpoint)
  }
}
