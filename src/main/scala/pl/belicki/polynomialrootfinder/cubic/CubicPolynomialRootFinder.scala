package pl.belicki.polynomialrootfinder.cubic

import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder
import pl.belicki.polynomialrootfinder.quadratic.QuadraticPolynomial
import pl.belicki.polynomialrootfinder.quadratic.QuadraticPolynomialRootFinder

object CubicPolynomialRootFinder extends PolynomialRootFinder[CubicPolynomial, QuadraticPolynomial] {

  override protected def findRootsIfLegit(polynomial: CubicPolynomial): List[Double] = {
    val A = ???
    val phi = ???
    val sigma = ???
    val B = ???
    val tau = ???
    val R3minus = ???
    val b = ???
    val R3plus = ???

    ???
  }

  override protected def fallbackFinder: PolynomialRootFinder[QuadraticPolynomial, _] = QuadraticPolynomialRootFinder

  override protected def constructLowerPolynomial(polynomial: CubicPolynomial): QuadraticPolynomial = 
    QuadraticPolynomial(polynomial.b, polynomial.c, polynomial.d)

}
