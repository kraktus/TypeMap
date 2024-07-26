package typemap

import scala.quoted.*
import scala.compiletime.*

// from https://stackoverflow.com/questions/70026258/scala-3-macros-how-do-i-get-name-of-generic-type-with-quoted-type
inline def typeName[A]: String = ${ typeNameImpl[A] }
def typeNameImpl[A: Type](using Quotes): Expr[String] =
  Expr(quotes.reflect.TypeRepr.of[A].dealiasKeepOpaques.show)

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
