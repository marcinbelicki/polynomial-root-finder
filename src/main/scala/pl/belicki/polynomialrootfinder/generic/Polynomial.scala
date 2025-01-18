package pl.belicki.polynomialrootfinder.generic

import scala.annotation.tailrec

trait Polynomial {

  def a: Double

  def isNotLegit: Boolean = a == 0

  def isLegit: Boolean = !isNotLegit

  def coefficients: List[Double]

  def calculateValue(x: Double): Double = {

    @tailrec
    def helper(
        coefficients: List[Double],
        exponent: Int,
        value: Double
    ): Double = coefficients match {
      case head :: tail =>
        helper(
          coefficients = tail,
          exponent = exponent + 1,
          value = value + head * Math.pow(x, exponent)
        )
      case _ => value
    }

    coefficients.reverse match {
      case head :: tail => helper(tail, 1, head)
      case _            => 0
    }
  }
}
