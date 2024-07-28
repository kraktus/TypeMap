import examples.*

import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global

class ExamplesTest extends munit.FunSuite:

  test("Bus") {
    val a                      = A(1)
    var aResult: Option[A]     = None
    val b                      = B("This is a B")
    var bResult: Option[B]     = None
    val c                      = C(3, "4", 5.0f)
    var cResult: Option[C]     = None
    val foo                    = Foo.Baz("baz")
    var fooResult: Option[Foo] = None
    Bus.subscribe[A] { case x: A => aResult = Some(x) }
    Bus.subscribe[B] { case y: B => bResult = Some(b) }
    Bus.subscribe[C] { case C(i, s, f) => cResult = Some(C(i, s, f)) }
    Bus.subscribe[D] { case D(init, p) => p.completeWith(Future.successful(init + 42)) }
    Bus.subscribe[Foo] { case Foo.Bar(i) => fooResult = Some(Foo.Bar(i)) }
    Bus.publish(a)
    Bus.publish(b)
    Bus.publish(c)
    Bus.publish(foo)
    assertEquals(aResult, Some(a))
    assertEquals(bResult, Some(b))
    assertEquals(cResult, Some(c))
    assertEquals(fooResult, None)
    Bus.subscribe[Foo] { case Foo.Baz(s) => fooResult = Some(Foo.Baz(s)) }
    Bus.publish(foo)
    assertEquals(fooResult, Some(foo))
    Bus.ask[Int, D](D(6, _)).foreach { x => assertEquals(x, 48) }
  }
