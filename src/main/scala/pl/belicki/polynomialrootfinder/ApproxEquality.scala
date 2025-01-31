package pl.belicki.polynomialrootfinder

object ApproxEquality {

  private val threshold = 1e-17

  def equals(x: Double, y: Double): Boolean =
    Math.abs(x - y) < threshold

}
