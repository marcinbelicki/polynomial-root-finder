package pl.belicki.polynomialrootfinder.quatric

import org.scalatest.matchers.must.Matchers.empty
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.wordspec.AnyWordSpecLike

class QuatricPolynomialRootFinderTest extends AnyWordSpecLike {

  "QuatricPolynomialRootFinder" must {
    "find no roots" in {

      val quatricPolynomial = QuatricPolynomial(
        a = 1,
        b = 2,
        c = 3,
        d = 4,
        e = 5
      )

      QuatricPolynomialRootFinder.findRoots(quatricPolynomial) shouldBe empty

    }

    "find two roots" in {

      val quatricPolynomial = QuatricPolynomial(
        a = 1,
        b = 2,
        c = 3,
        d = 4,
        e = - 5
      )

      QuatricPolynomialRootFinder.findRoots(quatricPolynomial) shouldBe empty

    }
  }

}
