package pl.belicki.polynomialrootfinder.cubic

import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder
import pl.belicki.polynomialrootfinder.quadratic.QuadraticPolynomial
import pl.belicki.polynomialrootfinder.quadratic.QuadraticPolynomialRootFinder

object CubicPolynomialRootFinder extends PolynomialRootFinder[CubicPolynomial, QuadraticPolynomial] {

  // https://community.ptc.com/sejnu66972/attachments/sejnu66972/PTCMathcad/103386/1/Article%20-On%20formulae%20for%20roots%20of%20cubic%20equation-.pdf
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
