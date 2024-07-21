import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global

case class A(i: Int)
case class B(s: String)
case class C(i: Int, s: String, f: Float)
case class D(init: Int, p: Promise[Int])

type ABCD = A | B | C | D

object Bus:
  val map: TypeMap[ABCD, Set[PartialFunction[ABCD, Unit]]] = TypeMap.empty

  inline def publish[T <: ABCD](t: T): Unit = map.get[T].foreach(_.foreach(_.apply(t)))
  inline def subscribe[T <: ABCD](f: PartialFunction[T, Unit]): Unit =
    val buseableFunction: PartialFunction[ABCD, Unit] = {
      case x: T =>
        f.applyOrElse(x, _ => println(s"Subscribe error: unhandled message of type ${typeName[T]}: $x"))
      case y => println(s"Subscribe error: Incorrect message type, wanted: ${typeName[T]}, received: $y")
    }
    map.put[T](map.get[T].fold(Set(buseableFunction))(_ + buseableFunction))

  inline def ask[A, T <: ABCD](makeMsg: Promise[A] => T)(using
      ExecutionContext
  ): Future[A] =
    val promise = Promise[A]()
    val msg     = makeMsg(promise)
    publish(msg)
    promise.future
