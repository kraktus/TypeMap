package examples
import typemap.{ TypeMap, MutableTypeMap, CMapBackend, assertBuseable, typeName, given }

import scala.reflect.ClassTag
import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.util.NotGiven

case class A(i: Int)
case class B(s: String)
case class C(i: Int, s: String, f: Float)
case class D(init: Int, p: Promise[Int])
case class E(zombo: String)
enum Foo:
  case Bar(i: Int)
  case Baz(s: String)

object Bus:
  private type Keys  = A | B | C | D | Foo
  private type Value = Set[PartialFunction[Keys, Unit]]

  val map: TypeMap[Keys, Value, CMapBackend] = TypeMap.empty

  inline def publish[T <: Keys](t: T)(using NotGiven[T <:< Tuple]): Unit =
    map.get[T].foreach(_.foreach(_.apply(t)))
  // extracted from `subscribe` to avoid warning about definition being duplicated at each callsite
  private def buseableFunctionBuilder[T <: Keys: ClassTag](
      f: PartialFunction[T, Unit]
  ): PartialFunction[Keys, Unit] = {
    case x: T =>
      // it's not always error when type T is enum, and matching only one variant
      f.applyOrElse(x, _ => ())
    // error because events are based by types
    case y => println(s"Subscribe error: Incorrect message type, wanted: ${typeName[T]}, received: $y")
  }

  inline def subscribe[T <: Keys: ClassTag](f: PartialFunction[T, Unit]): Unit =
    assertBuseable[T]
    val buseableFunction = buseableFunctionBuilder[T](f)
    map.compute[T](_.fold(Set(buseableFunction))(_ + buseableFunction))

  inline def ask[A, T <: Keys](makeMsg: Promise[A] => T)(using
      ExecutionContext
  ): Future[A] =
    val promise = Promise[A]()
    val msg     = makeMsg(promise)
    publish(msg)
    promise.future

object MutBus:
  private type Value = Set[PartialFunction[Any, Unit]]
  val map: MutableTypeMap[Value, CMapBackend] = MutableTypeMap.empty

  inline def publish[T](t: T)(using NotGiven[T <:< Tuple]): Unit = map.get[T].foreach(_.foreach(_.apply(t)))

  // extracted from `subscribe` to avoid warning about definition being duplicated at each callsite
  private def buseableFunctionBuilder[T <: Any: ClassTag](
      f: PartialFunction[T, Unit]
  ): PartialFunction[Any, Unit] = {
    case x: T =>
      // it's not always error when type T is enum, and matching only one variant
      f.applyOrElse(x, _ => ())
    // error because events are based by types
    case y => println(s"Subscribe error: Incorrect message type, wanted: ${typeName[T]}, received: $y")
  }

  inline def subscribe[T <: Any: ClassTag](f: PartialFunction[T, Unit]): Unit =
    assertBuseable[T]
    val buseableFunction = buseableFunctionBuilder[T](f)
    map.compute[T](_.fold(Set(buseableFunction))(_ + buseableFunction))

  inline def subscribe[T <: Any: ClassTag](f: T => Unit): Unit =
    assertBuseable[T]
    val partialF: PartialFunction[T, Unit] = { case x => f(x) } 
    val buseableFunction = buseableFunctionBuilder[T](partialF)
    map.compute[T](_.fold(Set(buseableFunction))(_ + buseableFunction))

  inline def ask[A, T](makeMsg: Promise[A] => T)(using
      ExecutionContext
  ): Future[A] =
    val promise = Promise[A]()
    val msg     = makeMsg(promise)
    publish(msg)
    promise.future
