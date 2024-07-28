import typemap.{ *, given }

import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
opaque type IntOpaque = Int
type Alias            = (Int, String, Float)
type Union            = String | Int | Float
type UnionDup         = String | Int | Float | Int
case class OutsidePackage(i: Int);
package inside:
  case class InsidePackage(x: String)

class TypeMapTest extends munit.FunSuite:
  test("isInTuple") {
    assertEquals(isInTuple[Int, (String, Int)], true)
    assertEquals(isInTuple[Int, Alias], true)
    assertEquals(isInTuple[Int, (String, IntOpaque)], false)
  }

  val string = "java.lang.String" // runtime
  val int    = "scala.Int"
  val float  = "scala.Float"

  test("typeName") {
    assertEquals(typeName[Int], int)
    assertEquals(typeName[String], string)
    assertEquals(typeName[Float], float)
    assertEquals(typeName[OutsidePackage], "OutsidePackage")
    import inside.*
    assertEquals(typeName[InsidePackage], "inside.InsidePackage")
    assertEquals(typeName[Alias], "scala.Tuple3[scala.Int, scala.Predef.String, scala.Float]")
  }

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
    assertNoDiff(
      compileErrors("typeNamesUnion[Int]"),
      """error: Type scala.Int is not a union type
      compileErrors("typeNamesUnion[Int]"),
                  ^""".stripMargin
    )
  }
  test("Canonical Union") {
    // should compile fine
    isUnionCanonical[String | Int]
    isUnionCanonical[Union]
    assertNoDiff(
      compileErrors("isUnionCanonical[UnionDup]"),
      """error: Type scala.Int multiple times (CHECK ALIASES) in union scala.Predef.String | scala.Int | scala.Float | scala.Int
      compileErrors("isUnionCanonical[UnionDup]"),
                  ^""".stripMargin
    )
  }
  test("TypeMap, CMapBackend") {
    val map: TypeMap[Int | String, String, CMapBackend] = TypeMap.empty
    map.put[Int]("1")
    assertEquals(map.get[Int], Some("1"))
    assertEquals(map.get[String], None)
    // does not compile, as expected
    assertNoDiff(
      compileErrors("map.get[Float]"),
      """error: Type scala.Float not found in union scala.Int | scala.Predef.String
      compileErrors("map.get[Float]"),
                  ^""".stripMargin
    )
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

  test("MutableTypeMap") {
    val map: MutableTypeMap[Int, CMapBackend] = MutableTypeMap.empty
    map.put[String](1)
    map.put[Float](2)
    map.put[A](3)
    assertEquals(map.get[String], Some(1))
    assertEquals(map.get[Float], Some(2))
    assertEquals(map.get[A], Some(3))
    assertEquals(map.get[B], None)
  }
