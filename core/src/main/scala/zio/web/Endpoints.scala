package zio.web

/**
 * An `Endpoints[M, Ids]` represents an ordered collection of endpoints with identifiers `Ids` and minimum metadata `M`.
 */
sealed trait Endpoints[-M[+_], Ids0] { self =>
  type Ids = Ids0

  final def +[M1[+_] <: M[_]](endpoint: Endpoint[M1, _, _, _]): Endpoints[M1, Ids with endpoint.Id] = {
    type Head = Endpoint.Aux[M1, endpoint.Params, endpoint.Input, endpoint.Output, endpoint.Id]
    type Tail = Endpoints[M1, self.Ids]

    Endpoints.Cons[M1, endpoint.Id, self.Ids, Head, Tail](
      endpoint.asInstanceOf[Head],
      self.asInstanceOf[Tail]
    )
  }
}

object Endpoints {

  final case class Cons[M1[+_], Id, Ids, E <: Endpoint.Aux[M1, _, _, _, Id], T <: Endpoints[M1, Ids]] private[web] (
    endpoint: E,
    tail: T
  ) extends Endpoints[M1, Ids with Id]

  sealed trait Empty extends Endpoints[Any, Any]

  private[web] case object Empty extends Empty

  def apply[M1[+_]](e1: Endpoint[M1, _, _, _]): Endpoints[M1, e1.Id] =
    empty + e1

  def apply[M1[+_]](
    e1: Endpoint[M1, _, _, _],
    e2: Endpoint[M1, _, _, _]
  ): Endpoints[M1, e1.Id with e2.Id] =
    empty + e1 + e2

  val empty: Empty = Empty
}
