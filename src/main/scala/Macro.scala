import scala.quoted.*
import scala.compiletime.*

// from https://stackoverflow.com/questions/70026258/scala-3-macros-how-do-i-get-name-of-generic-type-with-quoted-type
inline def typeName[A]: String = ${ typeNameImpl[A] }
private def typeNameImpl[A](using Type[A], Quotes): Expr[String] =
  Expr(Type.show[A])

inline def typeNamesTuple[T <: Tuple]: List[String] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => typeName[t].toString :: typeNamesTuple[ts]

// same function as typeNamesTuple, but as macro
inline def typeNamesTupleMacro[T <: Tuple]: List[String] = ${ typeNamesTupleMacroImpl[T] }
private def typeNamesTupleMacroImpl[T <: Tuple](using Quotes, Type[T]): Expr[List[String]] =
  import quotes.reflect.*
  val tTypes = TypeRepr.of[T].dealiasKeepOpaques match
    case AppliedType(_, tpes) => tpes
    case _                    => Nil
  Expr(tTypes.map(_.dealiasKeepOpaques.show))

inline def isInTuple[X, T <: Tuple]: Boolean = ${ isInTupleImpl[X, T] }

def isInTupleImpl[X: Type, T <: Tuple: Type](using Quotes): Expr[Boolean] =
  import quotes.reflect.*
  val xType = TypeRepr.of[X].dealiasKeepOpaques
  val tTypes = TypeRepr.of[T].dealiasKeepOpaques match
    case AppliedType(_, tpes) => tpes
    case _                    => Nil
  Expr(tTypes.exists(_ =:= xType))


// A macro named `createClass` that, inputting a list of fields as strings, create a class
// with a constructor that takes a value for each string in the list.

// example: `createClass[V](List("A", "B", "C"))`
// output: `class CreatedClass(val a: V, val b: V, val c: V)`
// NOT possible https://contributors.scala-lang.org/t/scala-3-macro-annotations-and-code-generation/6035
// inline def createClass[V](fields: List[String]): Any = ${ createClassImpl[V]('fields) }
// private def createClassImpl[V: Type](fields: Expr[List[String]])(using Quotes): Expr[Any] =
//   import quotes.reflect.*
//   val fieldTypes = fields.valueOrError.map { field => TypeRepr.of[V] }
//   val classDef = TypeDef(
//     Symbol.classSymbol("CreatedClass"),
//     TypeDef(
//       Symbol.classSymbol("val"),
//       fields.valueOrError.zip(fieldTypes).map { case (field, fieldType) =>
//       // TypeDef(Symbol.classSymbol(field), fieldType)
//       // [error]    |                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//       // [error]    |                Found:    (x$2.reflect.Symbol, x$2.reflect.TypeRepr)
//       // [error]    |                Required: x$2.reflect.Symbol
//         //TypeDef(Symbol.classSymbol(field), fieldType)
//       }
//     )
//   )
//   Expr(classDef)

inline def testUsingHMinMacro: Int = ${ testUsingHMinMacroImpl }
def testUsingHMinMacroImpl(using Quotes): Expr[Int] =
  import quotes.reflect.*
  val hm = new java.util.concurrent.ConcurrentHashMap[String, Int]()
  hm.put("a", 1)
  Expr(hm.get("a"))


// Question: how to keep global state between macro calls?
// Answer with code example: use a `val` in the companion object of the macro
