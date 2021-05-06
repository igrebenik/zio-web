package zio.web.websockets.codec

final case class BitsLength(min: Int, max: Option[Int]) { self =>

  def ++(that: BitsLength): BitsLength =
    BitsLength(
      self.min + that.min,
      for {
        l <- self.max
        r <- that.max
      } yield l + r
    )

  def atLeast: BitsLength = BitsLength(self.min, None)

  def atMost: BitsLength = BitsLength(0, self.max)
}

object BitsLength {

  val empty: BitsLength = BitsLength(0, None)

  def exact(n: Int): BitsLength = BitsLength(n, Some(n))

  def atLeast(n: Int): BitsLength = BitsLength(n, None)

  def atMost(n: Int): BitsLength = BitsLength(0, Some(n))

  def bounded(lower: Int, upper: Int): BitsLength = BitsLength(lower, Some(upper))

}
