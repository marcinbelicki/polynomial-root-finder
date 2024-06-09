package pl.belicki.polynomialrootfinder.util.complex

case class Complex(
    x: Double,
    y: Double
) {
  def +(that: Complex): Complex = Complex(x + that.x, y + that.y)
  def -(that: Complex): Complex = Complex(x - that.x, y - that.y)

  def *(that: Complex): Complex =
    Complex(x * that.x - y * that.y, x * that.y + y * that.x)

  def /(that: Complex): Complex = {
    require(that.x != 0 || that.y != 0)

    val denominator = Math.pow(that.x, 2) + Math.pow(that.y, 2)

    val realPartNumerator = x * that.x + y * that.y
    val imaginaryPartNumerator = y * that.x - x * that.y

    Complex(
      x = realPartNumerator / denominator,
      y = imaginaryPartNumerator / denominator
    )
  }

  def +[NUM: Numeric](that: NUM): Complex = {
    val numeric = implicitly[Numeric[NUM]]

    val double = numeric.toDouble(that)

    copy(x = x + double)
  }

  def -[NUM: Numeric](that: NUM): Complex = {
    val numeric = implicitly[Numeric[NUM]]

    val double = numeric.toDouble(that)

    copy(x = x - double)
  }

  def *[NUM: Numeric](that: NUM): Complex = {
    val numeric = implicitly[Numeric[NUM]]

    val double = numeric.toDouble(that)

    Complex(
      x = x * double,
      y = y * double
    )
  }

  def /[NUM: Numeric](that: NUM): Complex = {
    val numeric = implicitly[Numeric[NUM]]
    require(that != numeric.zero)

    val double = numeric.toDouble(that)

    Complex(
      x = x / double,
      y = y / double
    )
  }

  def equals[NUM: Numeric](num: NUM): Boolean =
    y == 0 && implicitly[Numeric[NUM]].toDouble(num) == x
  
}

object Complex {

  def arg(complex: Complex): Double = Math.atan2(complex.y, complex.x)

  def abs(complex: Complex): Double =
    Math.sqrt(Math.pow(complex.x, 2) + Math.pow(complex.y, 2))

  def exp(complex: Complex): Complex = {
    val realPartExp = Math.exp(complex.x)

    exponential(realPartExp, complex.y)
  }

  def expImaginary(imaginary: Double): Complex =
    apply(
      x = Math.cos(imaginary),
      y = Math.sin(imaginary)
    )

  def real(complex: Complex): Double = complex.x

  def imaginary(complex: Complex): Double = complex.y

  def pow(complex: Complex, power: Int) = {
    val abs = Math.pow(this.abs(complex), power)
    val arg = this.arg(complex) * power

    exponential(abs, arg)
  }

  def exponential(abs: Double, arg: Double): Complex =
    apply(
      x = Math.cos(arg) * abs,
      y = Math.sin(arg) * abs
    )

  implicit class RealAsComplex[NUM: Numeric](real: NUM) {
    private def numeric = implicitly[Numeric[NUM]]
    private def double = numeric.toDouble(real)

    def +(complex: Complex): Complex = complex.copy(x = complex.x + double)

    def -(complex: Complex): Complex = Complex(
      x = double - complex.x,
      y = -complex.y
    )

    def *(complex: Complex): Complex = complex * real

    def /(complex: Complex): Complex = {
      require(complex.x != 0 || complex.y != 0)

      val denominator = Math.pow(complex.x, 2) + Math.pow(complex.y, 2)

      val multiplier = double / denominator

      apply(
        x = complex.x * multiplier,
        y = complex.y * multiplier
      )
    }

  }

  def real[NUM: Numeric](num: NUM): Complex = apply(
    x = implicitly[Numeric[NUM]].toDouble(num),
    y = 0
  )

  def imaginary[NUM: Numeric](num: NUM): Complex = apply(
    x = 0,
    y = implicitly[Numeric[NUM]].toDouble(num)
  )

  val i = imaginary(1)

}
