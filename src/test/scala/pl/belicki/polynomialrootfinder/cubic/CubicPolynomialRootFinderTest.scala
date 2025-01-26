package pl.belicki.polynomialrootfinder.cubic

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers._

class CubicPolynomialRootFinderTest extends AnyWordSpecLike {

  "CubicPolynomialRootFinder" must {
    "properly calculate the result for simple case" in {
      CubicPolynomialRootFinder.findRootsOfLegitPolynomial(
        CubicPolynomial(
          a = 1,
          b = 0,
          c = 0,
          d = 0
        )
      ) shouldBe List(0)

      CubicPolynomialRootFinder.findRootsOfLegitPolynomial(
        CubicPolynomial(
          a = 1,
          b = 0,
          c = 0,
          d = -1
        )
      ) shouldBe List(1)
    }

    "properly calculate the result for more complex case" in {
      CubicPolynomialRootFinder.findRootsOfLegitPolynomial(
        CubicPolynomial(
          a = 1,
          b = 1,
          c = 1,
          d = 1
        )
      ) shouldBe List(-1)

      CubicPolynomialRootFinder.findRootsOfLegitPolynomial(
        CubicPolynomial(
          a = -0.4,
          b = -0.9,
          c = 1,
          d = 1
        )
      ) shouldBe List(
        -2.8219738551668874,
        -0.6977270845301631,
        1.2697009396970507
      )

    }
  }
}
