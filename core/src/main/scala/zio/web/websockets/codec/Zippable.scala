package zio.web.websockets.codec

sealed trait Zippable[A, B] {
  type Out

  def zip(left: A, right: B): Out

  def left(t: Out): A

  def right(t: Out): B
}

object Zippable extends LowPriorityImplicits {

  type Out[A, B, Z] = Zippable[A, B] { type Out = Z }

  implicit def Zippable3[A1, A2, Z]: Zippable.Out[(A1, A2), Z, (A1, A2, Z)] =
    new Zippable[(A1, A2), Z] {
      type Out = (A1, A2, Z)

      override def zip(left: (A1, A2), right: Z): (A1, A2, Z) = (left._1, left._2, right)

      override def left(t: (A1, A2, Z)): (A1, A2) = (t._1, t._2)

      override def right(t: (A1, A2, Z)): Z = t._3
    }

  implicit def Zippable4[A1, A2, A3, Z]: Zippable.Out[(A1, A2, A3), Z, (A1, A2, A3, Z)] =
    new Zippable[(A1, A2, A3), Z] {
      type Out = (A1, A2, A3, Z)

      override def zip(left: (A1, A2, A3), right: Z): (A1, A2, A3, Z) =
        (left._1, left._2, left._3, right)

      override def left(t: (A1, A2, A3, Z)): (A1, A2, A3) = (t._1, t._2, t._3)

      override def right(t: (A1, A2, A3, Z)): Z = t._4
    }

  implicit def Zippable5[A1, A2, A3, A4, Z]: Zippable.Out[(A1, A2, A3, A4), Z, (A1, A2, A3, A4, Z)] =
    new Zippable[(A1, A2, A3, A4), Z] {
      type Out = (A1, A2, A3, A4, Z)

      override def zip(left: (A1, A2, A3, A4), right: Z): (A1, A2, A3, A4, Z) =
        (left._1, left._2, left._3, left._4, right)

      override def left(t: (A1, A2, A3, A4, Z)): (A1, A2, A3, A4) = (t._1, t._2, t._3, t._4)

      override def right(t: (A1, A2, A3, A4, Z)): Z = t._5
    }

  implicit def Zippable6[A1, A2, A3, A4, A5, Z]: Zippable.Out[(A1, A2, A3, A4, A5), Z, (A1, A2, A3, A4, A5, Z)] =
    new Zippable[(A1, A2, A3, A4, A5), Z] {
      type Out = (A1, A2, A3, A4, A5, Z)

      override def zip(left: (A1, A2, A3, A4, A5), right: Z): (A1, A2, A3, A4, A5, Z) =
        (left._1, left._2, left._3, left._4, left._5, right)

      override def left(t: (A1, A2, A3, A4, A5, Z)): (A1, A2, A3, A4, A5) = (t._1, t._2, t._3, t._4, t._5)

      override def right(t: (A1, A2, A3, A4, A5, Z)): Z = t._6
    }

  implicit def Zippable7[A1, A2, A3, A4, A5, A6, Z]
    : Zippable.Out[(A1, A2, A3, A4, A5, A6), Z, (A1, A2, A3, A4, A5, A6, Z)] =
    new Zippable[(A1, A2, A3, A4, A5, A6), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, Z)

      override def zip(left: (A1, A2, A3, A4, A5, A6), right: Z): (A1, A2, A3, A4, A5, A6, Z) =
        (left._1, left._2, left._3, left._4, left._5, left._6, right)

      override def left(t: (A1, A2, A3, A4, A5, A6, Z)): (A1, A2, A3, A4, A5, A6) =
        (t._1, t._2, t._3, t._4, t._5, t._6)

      override def right(t: (A1, A2, A3, A4, A5, A6, Z)): Z = t._7
    }

  implicit def Zippable8[A1, A2, A3, A4, A5, A6, A7, Z]
    : Zippable.Out[(A1, A2, A3, A4, A5, A6, A7), Z, (A1, A2, A3, A4, A5, A6, A7, Z)] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, Z)

      override def zip(left: (A1, A2, A3, A4, A5, A6, A7), right: Z): (A1, A2, A3, A4, A5, A6, A7, Z) =
        (left._1, left._2, left._3, left._4, left._5, left._6, left._7, right)

      override def left(t: (A1, A2, A3, A4, A5, A6, A7, Z)): (A1, A2, A3, A4, A5, A6, A7) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, Z)): Z = t._8
    }

  implicit def Zippable9[A1, A2, A3, A4, A5, A6, A7, A8, Z]
    : Zippable.Out[(A1, A2, A3, A4, A5, A6, A7, A8), Z, (A1, A2, A3, A4, A5, A6, A7, A8, Z)] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, Z)

      override def zip(left: (A1, A2, A3, A4, A5, A6, A7, A8), right: Z): (A1, A2, A3, A4, A5, A6, A7, A8, Z) =
        (left._1, left._2, left._3, left._4, left._5, left._6, left._7, left._8, right)

      override def left(t: (A1, A2, A3, A4, A5, A6, A7, A8, Z)): (A1, A2, A3, A4, A5, A6, A7, A8) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, Z)): Z = t._9
    }

  implicit def Zippable10[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]
    : Zippable.Out[(A1, A2, A3, A4, A5, A6, A7, A8, A9), Z, (A1, A2, A3, A4, A5, A6, A7, A8, A9, Z)] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, Z)

      override def zip(left: (A1, A2, A3, A4, A5, A6, A7, A8, A9), right: Z): (A1, A2, A3, A4, A5, A6, A7, A8, A9, Z) =
        (left._1, left._2, left._3, left._4, left._5, left._6, left._7, left._8, left._9, right)

      override def left(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, Z)): (A1, A2, A3, A4, A5, A6, A7, A8, A9) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, Z)): Z = t._10
    }

  implicit def Zippable11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]
    : Zippable.Out[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Z, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z)] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z) =
        (left._1, left._2, left._3, left._4, left._5, left._6, left._7, left._8, left._9, left._10, right)

      override def left(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z)): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z)): Z = t._11
    }

  implicit def Zippable12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z) =
        (left._1, left._2, left._3, left._4, left._5, left._6, left._7, left._8, left._9, left._10, left._11, right)

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z)): Z = t._12
    }

  implicit def Zippable13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z)): Z = t._13
    }

  implicit def Zippable14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z)): Z = t._14
    }

  implicit def Zippable15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z)): Z = t._15
    }

  implicit def Zippable16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z)): Z = t._16
    }

  implicit def Zippable17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          left._16,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z)): Z = t._17
    }

  implicit def Zippable18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          left._16,
          left._17,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z)): Z = t._18
    }

  implicit def Zippable19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
    : Zippable.Out[
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18),
      Z,
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z)
    ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          left._16,
          left._17,
          left._18,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        (
          t._1,
          t._2,
          t._3,
          t._4,
          t._5,
          t._6,
          t._7,
          t._8,
          t._9,
          t._10,
          t._11,
          t._12,
          t._13,
          t._14,
          t._15,
          t._16,
          t._17,
          t._18
        )

      override def right(t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z)): Z =
        t._19
    }

  implicit def Zippable20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]
    : Zippable.Out[
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19),
      Z,
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z)
    ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          left._16,
          left._17,
          left._18,
          left._19,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        (
          t._1,
          t._2,
          t._3,
          t._4,
          t._5,
          t._6,
          t._7,
          t._8,
          t._9,
          t._10,
          t._11,
          t._12,
          t._13,
          t._14,
          t._15,
          t._16,
          t._17,
          t._18,
          t._19
        )

      override def right(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z)
      ): Z = t._20
    }

  implicit def Zippable21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]
    : Zippable.Out[
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20),
      Z,
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z)
    ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          left._16,
          left._17,
          left._18,
          left._19,
          left._20,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        (
          t._1,
          t._2,
          t._3,
          t._4,
          t._5,
          t._6,
          t._7,
          t._8,
          t._9,
          t._10,
          t._11,
          t._12,
          t._13,
          t._14,
          t._15,
          t._16,
          t._17,
          t._18,
          t._19,
          t._20
        )

      override def right(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z)
      ): Z = t._21
    }

  implicit def Zippable22[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    Z
  ]: Zippable.Out[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21),
    Z,
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z)
  ] =
    new Zippable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), Z] {
      type Out = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z)

      override def zip(
        left: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21),
        right: Z
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z) =
        (
          left._1,
          left._2,
          left._3,
          left._4,
          left._5,
          left._6,
          left._7,
          left._8,
          left._9,
          left._10,
          left._11,
          left._12,
          left._13,
          left._14,
          left._15,
          left._16,
          left._17,
          left._18,
          left._19,
          left._20,
          left._21,
          right
        )

      override def left(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z)
      ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        (
          t._1,
          t._2,
          t._3,
          t._4,
          t._5,
          t._6,
          t._7,
          t._8,
          t._9,
          t._10,
          t._11,
          t._12,
          t._13,
          t._14,
          t._15,
          t._16,
          t._17,
          t._18,
          t._19,
          t._20,
          t._21
        )

      override def right(
        t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z)
      ): Z = t._22
    }

}

trait LowPriorityImplicits {
  implicit def Zippable2[A, Z]: Zippable.Out[A, Z, (A, Z)] =
    new Zippable[A, Z] {
      type Out = (A, Z)

      override def zip(left: A, right: Z): Out = (left, right)

      override def left(t: Out): A = t._1

      override def right(t: Out): Z = t._2
    }
}
