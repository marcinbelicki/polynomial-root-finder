package pl.belicki.polynomialrootfinder.quatric

import pl.belicki.polynomialrootfinder.cubic.{
  CubicPolynomial,
  CubicPolynomialRootFinder
}
import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder

object QuatricPolynomialRootFinder
    extends PolynomialRootFinder[QuatricPolynomial, CubicPolynomial] {

  // https://www.sciencedirect.com/science/article/abs/pii/B9780080112206500068
  override protected def findRootsIfLegit(
      polynomial: QuatricPolynomial
  ): List[Double] = {
    val (neumarkA, neumarkB, neumarkC, neumarkD, neumarkE) =
      if (polynomial.a > 0)
        (
          polynomial.a,
          polynomial.b,
          polynomial.c,
          polynomial.d,
          polynomial.e
        )
      else
        (
          -polynomial.a,
          -polynomial.b,
          -polynomial.c,
          -polynomial.d,
          -polynomial.e
        )

    val resolventCubicPolynomial = CubicPolynomial(
      a = 1,
      b = 2 * neumarkC,
      c = Math.pow(neumarkC, 2) + neumarkB * ,
      d = ???
    )

  }

  override protected def fallbackFinder
      : PolynomialRootFinder[CubicPolynomial, _] = CubicPolynomialRootFinder

  override protected def constructLowerPolynomial(
      polynomial: QuatricPolynomial
  ): CubicPolynomial = CubicPolynomial(
    a = polynomial.b,
    b = polynomial.c,
    c = polynomial.d,
    d = polynomial.e
  )
}
