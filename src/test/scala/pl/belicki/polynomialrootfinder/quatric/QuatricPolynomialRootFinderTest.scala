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
        e = -5
      )

      QuatricPolynomialRootFinder.findRoots(quatricPolynomial) shouldBe List(
        0.6841243194530686,
        -2.059142444568353
      )

    }

    "find four roots" in {
      val quatricPolynomial = QuatricPolynomial(
        a = -0.6,
        b = -1.5,
        c = 5.6,
        d = 1,
        e = -4.4
      )

      QuatricPolynomialRootFinder.findRoots(quatricPolynomial) shouldBe
        List(
          -0.9058249943284854,
          -4.435486608748547,
          1.8600192484925686,
          0.981292354584464
        )

    }

    "find two roots in simple case" in {

      val quatricPolynomial = QuatricPolynomial(
        a = 1,
        b = 0,
        c = 0,
        d = 0,
        e = -1
      )

      QuatricPolynomialRootFinder.findRoots(quatricPolynomial) shouldBe
        List(
          1,
          -1
        )

    }
  }

}
