trait Busable

case class A(i: Int)                      extends Busable
case class B(s: String)                   extends Busable
case class C(i: Int, s: String, f: Float) extends Busable

type ABC = (A, B, C)

object Bus:
  val map: TypeMap[ABC, Set[PartialFunction[Busable, Unit]]] = TypeMap.empty

  inline def publish[T <: Busable](t: T): Unit = map.get[T].foreach(_.foreach(_.apply(t)))
  inline def subscribe[T <: Busable](f: PartialFunction[T, Unit]): Unit =
    val buseableFunction: PartialFunction[Busable, Unit] = {
      case x: T =>
        f.applyOrElse(x, _ => println(s"Subscribe error: unhandled message of type ${typeName[T]}: $x"))
      case y => println(s"Subscribe error:  is NOT message of type ${typeName[T]}: $y")
    }
    map.put[T](map.get[T].fold(Set(buseableFunction))(_ + buseableFunction))
