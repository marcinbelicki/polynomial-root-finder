package pl.belicki.polynomialrootfinder.quadratic

import pl.belicki.polynomialrootfinder.generic.Polynomial

case class QuadraticPolynomial(
  a: Double,
  b: Double,
  c: Double
) extends Polynomial {

  override lazy val coefficients: List[Double] = List(a, b, c)
}
