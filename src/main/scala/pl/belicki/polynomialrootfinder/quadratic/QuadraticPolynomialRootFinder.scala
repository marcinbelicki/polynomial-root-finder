package pl.belicki.polynomialrootfinder.quadratic

import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder
import pl.belicki.polynomialrootfinder.linear.LinearPolynomial
import pl.belicki.polynomialrootfinder.linear.LinearPolynomialRootFinder

object QuadraticPolynomialRootFinder
    extends PolynomialRootFinder[QuadraticPolynomial, LinearPolynomial] {

  override protected def findRootsIfLegit(
      polynomial: QuadraticPolynomial
  ): List[Double] = {
    val Delta = Math.pow(polynomial.b, 2) - 4 * polynomial.a * polynomial.c
    lazy val sqrtOfDelta = Math.sqrt(Delta)

    if (Delta < 0) return Nil
    if (Delta == 0) return List(-0.5 * polynomial.b / polynomial.a)

    List(
      sqrtOfDelta,
      -sqrtOfDelta
    )
      .map(-polynomial.b + _)
      .map(_ * 0.5 / polynomial.a)
  }

  override protected def fallbackFinder
      : PolynomialRootFinder[LinearPolynomial, _] = LinearPolynomialRootFinder

  override protected def constructLowerPolynomial(
      polynomial: QuadraticPolynomial
  ): LinearPolynomial = LinearPolynomial(a = polynomial.b, b = polynomial.c)

}
