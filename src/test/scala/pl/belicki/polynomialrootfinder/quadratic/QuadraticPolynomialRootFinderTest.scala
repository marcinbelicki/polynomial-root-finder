package pl.belicki.polynomialrootfinder.quadratic

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers._

class QuadraticPolynomialRootFinderTest extends AnyWordSpecLike {

  "QuadraticRootFinder" must {

    "properly find two roots for given polynomial" in {
      val quadraticPolynomial = QuadraticPolynomial(a = 0.2, b = 1, c = 1)

      QuadraticPolynomialRootFinder.findRoots(
        quadraticPolynomial
      ) shouldBe List(-1.3819660112501053, -3.6180339887498945)
    }

    "properly find no roots for given polynomial" in {
      val quadraticPolynomial = QuadraticPolynomial(a = 0.4, b = -1.2, c = 1.5)

      QuadraticPolynomialRootFinder.findRoots(
        quadraticPolynomial
      ) shouldBe empty
    }

    "properly find only one root for the simple case" in {
      val quadraticPolynomial = QuadraticPolynomial(a = 1, b = 0, c = 0)

      QuadraticPolynomialRootFinder.findRoots(
        quadraticPolynomial
      ) shouldBe List(0)
    }

  }

}
