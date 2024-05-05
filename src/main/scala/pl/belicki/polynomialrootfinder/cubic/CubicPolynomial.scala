package pl.belicki.polynomialrootfinder.cubic

import pl.belicki.polynomialrootfinder.generic.Polynomial

case class CubicPolynomial(
  a: Double,
  b: Double,
  c: Double,
  d: Double
) extends Polynomial {

  override lazy val coefficients: List[Double] = List(a, b, c, d)

}
