import typemap.{ *, given }

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
opaque type IntOpaque = Int
type Alias            = (Int, String, Float)
type Union            = String | Int | Float
type UnionDup         = String | Int | Float | Int

case class OutsidePackage(i: Int);
package inside:
  case class InsidePackage(x: String)

case class A(i: Int)
case class B(s: String)

enum EnumType:
  case EnumMember(i: Int)
  case Baz(s: String)

val errorMessageTemplate =
  "The type %s should be case class, or enum (NOT enum member)"

class TypeMapTest extends munit.FunSuite:
  test("isInTuple") {
    assertEquals(isInTuple[Int, (String, Int)], true)
    assertEquals(isInTuple[Int, Alias], true)
    assertEquals(isInTuple[Int, (String, IntOpaque)], false)
  }

  val string    = "java.lang.String" // runtime
  val int       = "scala.Int"
  val float     = "scala.Float"
  val intOpaque = "test$package.IntOpaque"

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
    assertEquals(typeNamesTuple[(String, IntOpaque)], List(string, intOpaque))
    assertEquals(typeNamesTuple[Alias], List(int, string, float))
  }

  test("typeNamesTupleMacro") {
    assertEquals(typeNamesTupleMacro[(String, Int)], List(string, int))
    assertEquals(typeNamesTupleMacro[(String, IntOpaque)], List(string, intOpaque))
    assertEquals(typeNamesTupleMacro[Alias], List(int, string, float))
  }
  test("Union type") {
    assertEquals(typeNamesUnion[String | Int], List(string, int))
    assertEquals(typeNamesUnion[String | IntOpaque], List(string, intOpaque))
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
      """error: Type scala.Int found multiple times (CHECK ALIASES) in union scala.Predef.String | scala.Int | scala.Float | scala.Int
      compileErrors("isUnionCanonical[UnionDup]"),
                  ^""".stripMargin
    )
  }

  test("assertBuseable") {
    assertBuseable[A]
    assertBuseable[inside.InsidePackage]
    assertBuseable[EnumType]
    assertNoDiff(
      compileErrors("assertBuseable[IntOpaque]"),
      s"""error: ${errorMessageTemplate.format("test$package.IntOpaque")}
      compileErrors("assertBuseable[IntOpaque]"),
                  ^""".stripMargin
    )
    assertNoDiff(
      compileErrors("assertBuseable[Alias]"),
      s"""error: ${errorMessageTemplate.format("scala.Tuple3[scala.Int, scala.Predef.String, scala.Float]")}
      compileErrors("assertBuseable[Alias]"),
                  ^""".stripMargin
    )
    assertNoDiff(
      compileErrors("assertBuseable[Union]"),
      s"""error: ${errorMessageTemplate.format("scala.Predef.String | scala.Int | scala.Float")}
      compileErrors("assertBuseable[Union]"),
                  ^""".stripMargin
    )
    assertNoDiff(
      compileErrors("assertBuseable[EnumType.EnumMember]"),
      s"""error: ${errorMessageTemplate.format("EnumType.EnumMember")}
      compileErrors("assertBuseable[EnumType.EnumMember]"),
                  ^""".stripMargin
    )
  }

  private def typeMapTest[Backend[_]](using ops: MapOps[Backend, String]) =
    val map: TypeMap[Int | String, String, Backend] = TypeMap.empty
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
    map

  test("TypeMap, CMapBackend") {
    val map = typeMapTest[CMapBackend]
    assertEquals(map.unsafeMap.get(int), "1")
  }
  test("TypeMap, ArraySeqBackend") {
    val map = typeMapTest[ArraySeqBackend]
    assertEquals(map.unsafeMap.indexOf(Some("1")), 0)
  }
  test("TypeMap, CowArrayBackend") {
    val map = typeMapTest[CowArrayBackend]
    assertEquals(map.unsafeMap.get(0), Some("1"))
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
    assertEquals(map.unsafeMap.get(float), 2)
    map.computeIfPresent[String](x => Some(x + 1))
    assertEquals(map.get[String], Some(2))
    map.computeIfAbsent[IntOpaque](Some(15))
    assertEquals(map.get[IntOpaque], Some(15))
    // assertEquals(map.unsafeMap.get("zoo"), null.asInstanceOf[Int])
  }

  test("MutableTypeMap make") {
    val map: MutableTypeMap[Int, CMapBackend] = MutableTypeMap.make(10)
    map.put[String](1)
    map.put[Float](2)
    map.put[A](3)
    assertEquals(map.get[String], Some(1))
    assertEquals(map.get[Float], Some(2))
    assertEquals(map.get[A], Some(3))
    assertEquals(map.get[B], None)
  }
