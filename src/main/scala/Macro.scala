package typemap

import scala.quoted.*
import scala.compiletime.*

// from https://stackoverflow.com/questions/70026258/scala-3-macros-how-do-i-get-name-of-generic-type-with-quoted-type
inline def typeName[A]: String = ${ typeNameImpl[A] }
def typeNameImpl[A: Type](using Quotes): Expr[String] =
  Expr(quotes.reflect.TypeRepr.of[A].dealiasKeepOpaques.show)

/** Error at compile-time if the type is not a case class or enum. This is useful when typemap is used to
  * create a type-safe Bus, such as in scalalib To forbid the following error from happening
  * ```scala3
  * Bus.sub: // compilers implicitely fallback to `Matchable`
  *  case A(i)       => prinln!("triggered!")
  *  case B(_)       => prinln!("triggered!")
  *
  *  Bus.pub[A](A(0))
  *  Bus.pub[B](B("str"))
  *  ...
  *  // `"triggered!"` is never printed because the types do not sync
  *  // the subcriber is listening on `Matchable`, while the publisher
  *  // use a subtype, such as `A` or `B`.
  *
  *  // The correct way to do is
  *  Bus.sub: // uses `A`, even better to explicitely cast it
  *    case A(i)       => prinln!("triggered!")
  *  Bus.sub: // uses `B`, even better to explicitely cast it
  *    case B(_)       => prinln!("triggered!")
  * ```
  * `assertBuseable` is used in `sub` to make sure the mistake errors at compile-time
  */
inline def assertBuseable[A] = ${ assertBuseableImpl[A] }

def assertBuseableImpl[A: Type](using Quotes) =
  import quotes.reflect.*
  val tpe    = quotes.reflect.TypeRepr.of[A].dealiasKeepOpaques
  val flags  = tpe.typeSymbol.flags
  val isEnum = flags.is(Flags.Enum)
  // internally tuples are represented as case class, so need to be filtered out manually
  val isTuple     = tpe <:< TypeRepr.of[Tuple]
  val isCaseClass = (!isTuple && tpe.typeSymbol.isClassDef && flags.is(Flags.Case))
  if !(isEnum || isCaseClass) then report.errorAndAbort(s"The type ${tpe.show} should be case class, or enum")
  '{}

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

object MacroTest:
  inline def printFieldsAndTypes[T]: Unit = ${ printFieldsAndTypesImpl[T] }

  def printFieldsAndTypesImpl[T: Type](using Quotes): Expr[Unit] =
    import quotes.reflect.*

    // Get the type symbol of T
    val tpe    = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    // Ensure that T is a class
    if !symbol.isClassDef then
      report.error("Expected a class type")
      return '{ () }

    // Get all fields of the class
    val fields = symbol.primaryConstructor.paramSymss.flatten.map { param =>
      val name = param.name
      val fieldType = param.tree match
        case v: ValDef => v.tpt.tpe.show
        case _         => "unknown"
      (name, fieldType)
    }

    // Print the fields and their types
    fields.foreach { case (name, fieldType) =>
      println(s"Field: $name, Type: $fieldType")
    }

    '{ () }
