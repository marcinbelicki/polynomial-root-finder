package pl.belicki.polynomialrootfinder.linear

import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder

object LinearPolynomialRootFinder
    extends PolynomialRootFinder[LinearPolynomial, Nothing] {

  protected def findRootsIfLegit(polynomial: LinearPolynomial): List[Double] =
    List(-polynomial.b / polynomial.a)

  protected def fallbackFinder: PolynomialRootFinder[Nothing, Nothing] = ???

  protected def constructLowerPolynomial(
      polynomial: LinearPolynomial
  ): Nothing = ???
}
