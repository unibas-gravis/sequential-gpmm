package method.fittingTools

import breeze.linalg.{Axis, DenseMatrix, DenseVector, sum, tile}
import scalismo.common.{DiscreteDomain, PointId, Vectorizer}
import scalismo.geometry.{EuclideanVector, NDSpace, Point, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.MultivariateNormalDistribution

//Code from: https://github.com/madsendennis/template-registration-with-scala.git

object TemplateRegistrationEvaluationHelpers {

  object icp{

    def getIcpLabelMapData(template: TriangleMesh[_3D], target: TriangleMesh[_3D]): Seq[Double] = {
      val (corr, _) = closestPointCorrespondence(template, target)
      val w = corr.map(_._3)
      w
    }

    private def closestPointCorrespondence(template: TriangleMesh[_3D], target: TriangleMesh[_3D]): (Seq[(PointId, Point[_3D], Double)], Double) = {
      var distance = 0.0
      val corr = template.pointSet.pointIds.toSeq.map { id =>
        val p = template.pointSet.point(id)
        val closestPointOnSurface = target.operations.closestPointOnSurface(p)
        val closestPoint = target.pointSet.findClosestPoint(closestPointOnSurface.point)
        val w = if (isPointOnBoundary(closestPoint.id, target)) 0.0
        else if (isNormalDirectionOpposite(template.vertexNormals.atPoint(id), target.vertexNormals.atPoint(closestPoint.id))) 0.0
        else if (isClosestPointIntersecting(id, closestPointOnSurface.point, template)) 0.0
        else 1.0
        distance += closestPointOnSurface.distance
        (id, closestPointOnSurface.point, w)
      }
      (corr, distance / template.pointSet.numberOfPoints)
    }

    private def isPointOnBoundary(id: PointId, mesh: TriangleMesh[_3D]): Boolean = {
      mesh.operations.pointIsOnBoundary(id)
    }

    private def isNormalDirectionOpposite(n1: EuclideanVector[_3D], n2: EuclideanVector[_3D]): Boolean = {
      (n1 dot n2) < 0
    }

    private def isClosestPointIntersecting(id: PointId, cp: Point[_3D], mesh: TriangleMesh[_3D]): Boolean = {
      val p = mesh.pointSet.point(id)
      val v = p-cp
      val intersectingPoints = mesh.operations.getIntersectionPoints(p, v).filter(f => f != p) // All intersecting points with the closest point vector
      val closestIntersectingPoint = if (intersectingPoints.nonEmpty) intersectingPoints.map(ip => (p - ip).norm).min else Double.PositiveInfinity // Distance to closest intersecting point on template
      (closestIntersectingPoint < (v).norm)
    }
  }

  object cpd {

    //template is the fit from the registration
    //w is fraction of outliers
    def getCorrespondence(template: Seq[Point[_3D]], target: Seq[Point[_3D]], w: Double, sigma2: Double): DenseVector[Double] = {
      val P = Expectation(template, target, sigma2, w)
      val P1 = sum(P, Axis._1)
      P1
    }

    private def Expectation(template: Seq[Point[_3D]], target: Seq[Point[_3D]], sigma2: Double, w: Double): DenseMatrix[Double] = {
      val M = template.length
      val N = target.length
      // TODO: Approximate using nyström
      val P: DenseMatrix[Double] = DenseMatrix.zeros[Double](M, N)
      template.zipWithIndex.foreach { case (y, i) =>
        target.zipWithIndex.foreach { case (x, j) =>
          P(i, j) = gaussKernel(x, y, sigma2)
        }
      }
      val dim = 3.0
      val c = w / (1 - w) * math.pow((2.0 * math.Pi * sigma2), dim / 2.0) * (M.toDouble / N.toDouble)
      val denRow = DenseMatrix(sum(P, Axis._0).t)
      val den = tile(denRow, M, 1) + c

      P /:/ den
    }

    private def gaussKernel(x: Point[_3D], y: Point[_3D], sigma2: Double): Double = {
      math.exp(-(x - y).norm2 / (2.0 * sigma2))
    }



  }


}
