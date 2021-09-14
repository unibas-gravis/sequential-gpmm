package method.fittingTools

import breeze.numerics.{erf, erfinv, exp, sqrt}
import breeze.stats.distributions.Gaussian

case class HalfNormalDistribution(sigma: Double = 1.0) {

  private val normal = Gaussian(0.0, sigma)

  def pdf(x: Double): Double = sqrt(2) / (sigma * sqrt(3.14)) * exp(-(x * x) / 2.0)

  def cdf(x: Double): Double = erf(x / (sigma * sqrt(2.0)))

  def invCdf(f: Double): Double = sqrt(2) * sigma * erfinv(f)

  def sample(): Double = {
    val gaussianSample = normal.sample()
    val cumulativeP = normal.cdf(gaussianSample)
    invCdf(cumulativeP)
  }

  def getSigma(data: IndexedSeq[Double]): Double = {
    val mean = data.sum / data.length
    mean * sqrt(3.14) / sqrt(2.0)
  }

}
