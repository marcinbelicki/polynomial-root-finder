package pl.belicki.polynomialrootfinder.generic

import scala.annotation.tailrec

trait PolynomialRootFinder[
    POLYNOMIAL <: Polynomial,
    LOWER_POLYNOMIAL <: Polynomial
] {

  protected def findRootsIfLegit(polynomial: POLYNOMIAL): List[Double]

  protected def fallbackFinder: PolynomialRootFinder[LOWER_POLYNOMIAL, _]

  protected def constructLowerPolynomial(
      polynomial: POLYNOMIAL
  ): LOWER_POLYNOMIAL

  final def findRootsOfLegitPolynomial(polynomial: POLYNOMIAL): List[Double] = {
    require(polynomial.isLegit)
    findRootsIfLegit(polynomial)
  }

  @tailrec
  final def findRoots(polynomial: POLYNOMIAL): List[Double] =
    if (polynomial.isNotLegit)
      fallbackFinder.findRoots(constructLowerPolynomial(polynomial))
    else findRootsIfLegit(polynomial)

}
