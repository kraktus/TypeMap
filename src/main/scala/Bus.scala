import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.ConcurrentHashMap as CMap

case class A(i: Int)
case class B(s: String)
case class C(i: Int, s: String, f: Float)
case class D(init: Int, p: Promise[Int])

object Bus:
  private type Keys  = A | B | C | D
  private type Value = Set[PartialFunction[Keys, Unit]]
  val map: TypeMap[Keys, Value, CMapBackend] = TypeMap.empty

  inline def publish[T <: Keys](t: T): Unit = map.get[T].foreach(_.foreach(_.apply(t)))
  inline def subscribe[T <: Keys](f: PartialFunction[T, Unit]): Unit =
    val buseableFunction: PartialFunction[Keys, Unit] = {
      case x: T =>
        // it's not always error when type T is enum, and matching only one variant
        f.applyOrElse(x, _ => ())
      // error because events are based by types
      case y => println(s"Subscribe error: Incorrect message type, wanted: ${typeName[T]}, received: $y")
    }
    map.put[T](map.get[T].fold(Set(buseableFunction))(_ + buseableFunction))

  inline def ask[A, T <: Keys](makeMsg: Promise[A] => T)(using
      ExecutionContext
  ): Future[A] =
    val promise = Promise[A]()
    val msg     = makeMsg(promise)
    publish(msg)
    promise.future
