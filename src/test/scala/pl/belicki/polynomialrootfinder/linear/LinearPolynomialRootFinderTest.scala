package pl.belicki.polynomialrootfinder.linear

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers._

class LinearPolynomialRootFinderTest extends AnyWordSpecLike {

  "LinearPolynomialRootFinder" must {
    "find proper root of the linear polynomial" in {

      val linearPolynomial = LinearPolynomial(a = 5, b = 2)

      LinearPolynomialRootFinder.findRoots(linearPolynomial) shouldBe List(-0.4)

    }

    "throw an error when a = 0" in {

      val notLegitLinearPolynomial = LinearPolynomial(a = 0, b = 1)

      a[NotImplementedError] shouldBe thrownBy(
        LinearPolynomialRootFinder.findRoots(notLegitLinearPolynomial)
      )

    }
  }
}
