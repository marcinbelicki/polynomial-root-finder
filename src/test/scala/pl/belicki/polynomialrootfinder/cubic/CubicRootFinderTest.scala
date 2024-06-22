package pl.belicki.polynomialrootfinder.cubic

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers._

class CubicRootFinderTest extends AnyWordSpecLike {

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
  }
}
