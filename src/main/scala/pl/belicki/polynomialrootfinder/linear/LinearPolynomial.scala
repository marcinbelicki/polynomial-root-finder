package pl.belicki.polynomialrootfinder.linear

import pl.belicki.polynomialrootfinder.generic.Polynomial

case class LinearPolynomial(
    a: Double,
    b: Double
) extends Polynomial {

    override def coefficients: List[Double] = List(a, b)

}
