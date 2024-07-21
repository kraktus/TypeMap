import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
opaque type IntOpaque = Int
type Alias            = (Int, String, Float)
type Union            = String | Int | Float

class MySuite extends munit.FunSuite:
  test("isInTuple") {
    assertEquals(isInTuple[Int, (String, Int)], true)
    assertEquals(isInTuple[Int, Alias], true)
    assertEquals(isInTuple[Int, (String, IntOpaque)], false)
  }

  val string = "java.lang.String" // runtime
  val int    = "scala.Int"
  val float  = "scala.Float"

  test("typeNamesTuple") {
    assertEquals(typeNamesTuple[(String, Int)], List(string, int))
    assertEquals(typeNamesTuple[(String, IntOpaque)], List(string, "MySuite$package.IntOpaque"))
    assertEquals(typeNamesTuple[Alias], List(int, string, float))
  }

  test("typeNamesTupleMacro") {
    assertEquals(typeNamesTupleMacro[(String, Int)], List(string, int))
    assertEquals(typeNamesTupleMacro[(String, IntOpaque)], List(string, "MySuite$package.IntOpaque"))
    assertEquals(typeNamesTupleMacro[Alias], List(int, string, float))
  }
  test("Union type") {
    assertEquals(typeNamesUnion[String | Int], List(string, int))
    assertEquals(typeNamesUnion[String | IntOpaque], List(string, "MySuite$package.IntOpaque"))
    assertEquals(typeNamesUnion[Int | String | Float], List(int, string, float))
    assertEquals(typeNamesUnion[Union], List(string, int, float))
    // does not compile, as expected
    // assertEquals(typeNamesUnion[Int], List())
  }
  test("TypeMap, CMapBackend") {
    val map: TypeMap[Int | String, String, CMapBackend] = TypeMap.empty
    map.put[Int]("1")
    assertEquals(map.get[Int], Some("1"))
    assertEquals(map.get[String], None)
    // does not compile, as expected
    // assertEquals(map.get[Float], None)
  }

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
