package pl.belicki.polynomialrootfinder.cubic

import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder
import pl.belicki.polynomialrootfinder.quadratic.QuadraticPolynomial
import pl.belicki.polynomialrootfinder.quadratic.QuadraticPolynomialRootFinder

object CubicPolynomialRootFinder
    extends PolynomialRootFinder[CubicPolynomial, QuadraticPolynomial] {

  // https://community.ptc.com/sejnu66972/attachments/sejnu66972/PTCMathcad/103386/1/Article%20-On%20formulae%20for%20roots%20of%20cubic%20equation-.pdf
  override protected def findRootsIfLegit(
      polynomial: CubicPolynomial
  ): List[Double] = {
    val tau = Math.pow(polynomial.b, 2) - 3 * polynomial.a * polynomial.c
    val q = polynomial.b
      .*(9 * polynomial.a * polynomial.c - 2 * Math.pow(polynomial.b, 2))
      .-(27 * Math.pow(polynomial.a, 2) * polynomial.d)

    lazy val sigma = Math.pow(3 * polynomial.a, -1)

    if (tau == 0) {
      val x1 = sigma * (Math.cbrt(q) - polynomial.b)

      if (polynomial.b == 0) return List(x1)
      if (q == 0) return List(x1, -sigma * 0.5 * polynomial.b)

      return List(x1)
    }

    if (tau < 0) {
      val B = -q / (2 * tau * Math.sqrt(Math.abs(tau)))

      val R = Math.abs(B) + Math.sqrt(Math.pow(B, 2) + 1)

      val R3minus = Math.cbrt(R) - 1 / Math.cbrt(R)
      val R3plus = Math.cbrt(R) + 1 / Math.cbrt(R)

      val x1 = sigma * (Math.signum(B) * Math.sqrt(
        Math.abs(tau)
      ) * R3minus - polynomial.b)

      lazy val realPart =
        -Math.signum(B) * Math.sqrt(
          Math.abs(tau)
        ) * 0.5 * R3minus * sigma - polynomial.b * sigma
      val imaginaryPart =
        Math.sqrt(3) * 0.5 * Math.sqrt(Math.abs(tau)) * R3plus * sigma

      if (imaginaryPart != 0) return List(x1)

      return List(x1, realPart)
    }

    val sqrtTau = Math.sqrt(tau)

    val alpha = 2 * tau * sqrtTau / (27 * Math.pow(polynomial.a, 2))
    val delta = q / (27 * Math.pow(polynomial.a, 2))

    val A = delta / alpha

    val absA = Math.abs(A)

    if (absA <= 1) {
      val phi = Math.acos(A)

      val c0 = Math.cos(phi / 3)
      val p = c0 * Math.sqrt(tau)
      val r =
        Math.sqrt(3) * Math.sqrt(tau) * Math.sqrt(Math.abs(1 - Math.pow(c0, 2)))

      lazy val x1 = sigma * (2 * p - polynomial.b)
      lazy val x2 = sigma * (r - p - polynomial.b)
      lazy val x3 = -sigma * (p + r + polynomial.b)

      if (A == 1) return List(x1, x2)
      if (A == -1) return List(x1, x3)

      return List(x1, x2, x3)
    }

    val R = absA + Math.sqrt(Math.abs(Math.pow(A, 2) - 1))

    val R3plus = Math.cbrt(R) + 1 / Math.cbrt(R)

    val x1 = sigma * (Math.signum(A) * Math.sqrt(tau) * R3plus - polynomial.b)

    lazy val realPart =
      (-sigma * Math.signum(A) * Math.sqrt(tau) * 0.5 * R3plus)
        .-(polynomial.b * sigma)

    lazy val imaginaryPart = Math.sqrt(3) * 0.5 * Math.sqrt(tau) * R3plus

    if (imaginaryPart != 0) return List(x1)

    List(x1, realPart)
  }

  override protected def fallbackFinder
      : PolynomialRootFinder[QuadraticPolynomial, _] =
    QuadraticPolynomialRootFinder

  override protected def constructLowerPolynomial(
      polynomial: CubicPolynomial
  ): QuadraticPolynomial =
    QuadraticPolynomial(polynomial.b, polynomial.c, polynomial.d)

}
