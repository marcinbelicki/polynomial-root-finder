package pl.belicki.polynomialrootfinder.quatric

import pl.belicki.polynomialrootfinder.cubic.{
  CubicPolynomial,
  CubicPolynomialRootFinder
}
import pl.belicki.polynomialrootfinder.generic.PolynomialRootFinder
import pl.belicki.polynomialrootfinder.quadratic.{
  QuadraticPolynomial,
  QuadraticPolynomialRootFinder
}

object QuatricPolynomialRootFinder
    extends PolynomialRootFinder[QuatricPolynomial, CubicPolynomial] {

  // https://www.sciencedirect.com/science/article/abs/pii/B9780080112206500068
  override protected def findRootsIfLegit(
      polynomial: QuatricPolynomial
  ): List[Double] = {
    if (polynomial.e == 0) {
      val cubicPolynomial = CubicPolynomial(
        a = polynomial.a,
        b = polynomial.b,
        c = polynomial.c,
        d = polynomial.d
      )
      val certainRoot = 0d

      return CubicPolynomialRootFinder
        .findRootsOfLegitPolynomial(
          cubicPolynomial
        )
        .prepended(certainRoot)
        .distinct
    }

    val neumark =
      if (polynomial.a > 0) polynomial
      else
        QuatricPolynomial(
          -polynomial.a,
          -polynomial.b,
          -polynomial.c,
          -polynomial.d,
          -polynomial.e
        )

    val resolventCubicPolynomial = CubicPolynomial(
      a = 1,
      b = -2 * neumark.c,
      c = Math
        .pow(neumark.c, 2) + neumark.b * neumark.d - 4 * neumark.a * neumark.e,
      d = -neumark.b * neumark.c * neumark.d + Math.pow(
        neumark.b,
        2
      ) * neumark.e + neumark.a * Math.pow(neumark.d, 2)
    )

    val cubicRoots = CubicPolynomialRootFinder
      .findRootsOfLegitPolynomial(
        resolventCubicPolynomial
      )

    def calculateInSqrt(x: Double): Double =
      Math.pow(neumark.b, 2) - 4 * neumark.a * x

    def calculateNeumarkCoefficients(x: Double) = {
      val inSqrt = calculateInSqrt(x)
      lazy val sqrt = Math.sqrt(inSqrt)

      Option.when(inSqrt >= 0) {
        NeumarkCoefficients(
          G = 0.5 * (neumark.b + sqrt),
          g = 0.5 * (neumark.b - sqrt),
          H =
            0.5 * (neumark.c - x + ((neumark.b * (neumark.c - x)) - 2 * neumark.a * neumark.d) / sqrt),
          h =
            0.5 * (neumark.c - x - ((neumark.b * (neumark.c - x)) - 2 * neumark.a * neumark.d) / sqrt)
        )
      }
    }

    def calculateQuadraticsForCoefficients(
        neumarkCoefficients: NeumarkCoefficients
    ) =
      List(
        QuadraticPolynomial(
          a = neumark.a,
          b = neumarkCoefficients.G,
          c = neumarkCoefficients.H
        ),
        QuadraticPolynomial(
          a = neumark.a,
          b = neumarkCoefficients.g,
          c = neumarkCoefficients.h
        )
      )

    val specialCaseRoot = 0.25 * Math.pow(neumark.b, 2) / neumark.a

    def quadratics: List[QuadraticPolynomial] = {

      val equality = cubicRoots
        .map(_ - specialCaseRoot)
        .map(Math.abs)
        .exists(_ < equalityThreshold)

      if (!equality)
        return cubicRoots.minOption
          .flatMap(calculateNeumarkCoefficients)
          .fold[List[QuadraticPolynomial]](Nil)(
            calculateQuadraticsForCoefficients
          )

      val inSqrt =
        Math.pow(neumark.c - specialCaseRoot, 2) - 4 * neumark.a * neumark.e

      lazy val sqrt = Math.sqrt(inSqrt)

      if (inSqrt < 0) return List.empty

      val neumarkCoefficients = NeumarkCoefficients(
        G =
          0.5 * (neumark.b + (neumark.b * (neumark.c - specialCaseRoot) - 2 * neumark.a * neumark.d)
            / sqrt),
        g =
          0.5 * (neumark.b - (neumark.b * (neumark.c - specialCaseRoot) - 2 * neumark.a * neumark.d)
            / sqrt),
        H = 0.5 * (neumark.c - specialCaseRoot + sqrt),
        h = 0.5 * (neumark.c - specialCaseRoot - sqrt)
      )

      calculateQuadraticsForCoefficients(neumarkCoefficients)
    }

    quadratics
      .flatMap(QuadraticPolynomialRootFinder.findRoots)
  }

  private val equalityThreshold = 1e-100

  private case class NeumarkCoefficients(
      G: Double,
      g: Double,
      H: Double,
      h: Double
  )

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
