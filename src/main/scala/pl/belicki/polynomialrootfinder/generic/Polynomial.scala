package pl.belicki.polynomialrootfinder.generic


trait Polynomial {

  def a: Double

  def isNotLegit: Boolean = a == 0

  def isLegit: Boolean = !isNotLegit
}
