package pl.belicki.polynomialrootfinder.quatric

import pl.belicki.polynomialrootfinder.generic.Polynomial

case class QuatricPolynomial(
    a: Double,
    b: Double,
    c: Double,
    d: Double,
    e: Double
) extends Polynomial {
  override lazy val coefficients: List[Double] = List(a, b, c, d, e)
}
