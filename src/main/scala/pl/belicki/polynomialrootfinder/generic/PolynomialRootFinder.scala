package pl.belicki.polynomialrootfinder.generic

trait PolynomialRootFinder[POLYNOMIAL <: Polynomial] {

  def findRoots(polynomial: POLYNOMIAL): List[Double]

}
