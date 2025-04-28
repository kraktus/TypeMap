package typemap

import scala.quoted.*
import scala.compiletime.*

// from https://stackoverflow.com/questions/70026258/scala-3-macros-how-do-i-get-name-of-generic-type-with-quoted-type
inline def typeName[A]: String = ${ typeNameImpl[A] }
def typeNameImpl[A: Type](using Quotes): Expr[String] =
  Expr(quotes.reflect.TypeRepr.of[A].dealiasKeepOpaques.show)

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
