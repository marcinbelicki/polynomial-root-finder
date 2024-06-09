package pl.belicki.polynomialrootfinder.util.complex

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers._
import Complex.i

class ComplexTest extends AnyWordSpecLike {

  "Complex numbers" must {
    "be added properly" in {
      4 + i * 2 + 4 + i * 2 shouldBe 8 + i * 4
    }

    "be subtracted properly" in {
      4 + i * 2 - (4 + i * 2) shouldBe Complex.real(0)
    }

    "be multiplied properly" in {
      (4 + i * 2) * (9 + i * 3) shouldBe 30 + 30 * i
    }

  }
}
