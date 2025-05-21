package examples
import typemap.{ TypeMap, MutableTypeMap, CMapBackend, assertBuseable, typeName, given }

import scala.reflect.Typeable
import scala.concurrent.{ Future, Promise }
import scala.util.NotGiven
import scala.annotation.nowarn

case class A(i: Int)
case class B(s: String)
case class C(i: Int, s: String, f: Float)
case class D(init: Int, p: Promise[Int])
case class E(zombo: String)
case class Impossible(x: String) extends NotBuseable
enum Foo:
  case Bar(i: Int)
  case Baz(s: String)

object Bus:
  private type Keys  = A | B | C | D | Foo
  private type Value = Set[PartialFunction[Keys, Unit]]

  val map: TypeMap[Keys, Value, CMapBackend] = TypeMap.empty

  inline def publish[T <: Keys](t: T)(using @nowarn ng: NotGiven[T <:< Tuple]): Unit =
    map.get[T].foreach(_.foreach(_.apply(t)))
  // extracted from `subscribe` to avoid warning about definition being duplicated at each callsite
  private def buseableFunctionBuilder[T <: Keys: Typeable](
      f: PartialFunction[T, Unit]
  ): PartialFunction[Keys, Unit] = {
    case x: T =>
      // it's not always error when type T is enum, and matching only one variant
      f.applyOrElse(x, _ => ())
    // error because events are based by types
    case y => println(s"Subscribe error: Incorrect message type, wanted: ${typeName[T]}, received: $y")
  }

  inline def subscribe[T <: Keys: Typeable](f: PartialFunction[T, Unit]): Unit =
    assertBuseable[T]
    val buseableFunction = buseableFunctionBuilder[T](f)
    map.compute[T](v => Some(v.fold(Set(buseableFunction))(_ + buseableFunction)))

  inline def ask[A, T <: Keys](makeMsg: Promise[A] => T): Future[A] =
    val promise = Promise[A]()
    val msg     = makeMsg(promise)
    publish(msg)
    promise.future

trait NotBuseable

object MutBus:
  private type Value = Set[PartialFunction[Any, Unit]]
  val map: MutableTypeMap[Value, CMapBackend] = MutableTypeMap.empty

  inline def publish[T](
      t: T
  )(using @nowarn ng1: NotGiven[T <:< Tuple], @nowarn ng2: NotGiven[T <:< NotBuseable]): Unit =
    map.get[T].foreach(_.foreach(_.apply(t)))

  // extracted from `subscribe` to avoid warning about definition being duplicated at each callsite
  private def buseableFunctionBuilder[T <: Any: Typeable](
      f: PartialFunction[T, Unit]
  ): PartialFunction[Any, Unit] = {
    case x: T =>
      // it's not always error when type T is enum, and matching only one variant
      f.applyOrElse(x, _ => ())
    // error because events are based by types
    case y => println(s"Subscribe error: Incorrect message type, wanted: ${typeName[T]}, received: $y")
  }

  inline def subscribe[T <: Any: Typeable](f: PartialFunction[T, Unit]): Unit =
    assertBuseable[T]
    val buseableFunction = buseableFunctionBuilder[T](f)
    map.compute[T](v => Some(v.fold(Set(buseableFunction))(_ + buseableFunction)))

  inline def ask[A, T](makeMsg: Promise[A] => T): Future[A] =
    val promise = Promise[A]()
    val msg     = makeMsg(promise)
    publish(msg)
    promise.future
