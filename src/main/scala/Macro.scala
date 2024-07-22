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

// same function as isInTuple, but for union types
inline def isInUnion[X, U]: Boolean = ${ isInUnionImpl[X, U] }

private def isInUnionImpl[X: Type, U: Type](using Quotes): Expr[Boolean] =
  import quotes.reflect.*
  // sanity check
  isUnionSanityCheck[U]

  val xType  = TypeRepr.of[X].dealiasKeepOpaques
  val u      = TypeRepr.of[U].dealiasKeepOpaques
  val uTypes = get(u)
  Expr(uTypes.exists(_ =:= xType))

def unionLengthImpl[U: Type](using Quotes): Expr[Int] =
  import quotes.reflect.*
  // sanity check
  isUnionSanityCheck[U]

  val uTypes = get(TypeRepr.of[U].dealiasKeepOpaques)
  Expr(uTypes.length)

inline def indexInUnion[X, U]: Int = ${ indexInUnionImpl[X, U] }

private def indexInUnionImpl[X: Type, U: Type](using Quotes): Expr[Int] =
  import quotes.reflect.*
  // sanity check
  isUnionSanityCheck[U]

  val xType  = TypeRepr.of[X].dealiasKeepOpaques
  val u      = TypeRepr.of[U].dealiasKeepOpaques
  val uTypes = get(u)
  val index = uTypes.zipWithIndex.find(_._1 =:= xType).map(_._2) match
    case Some(idx) => idx
    case None      => report.errorAndAbort(s"Type ${xType.show} not found in union ${u.show}")
  Expr(index)

private def isUnionSanityCheck[T: Type](using Quotes): Unit =
  import quotes.reflect.*
  val t = TypeRepr.of[T].dealiasKeepOpaques
  t match
    case OrType(_, _) => ()
    case x            => report.errorAndAbort(s"Type ${x.show} is not a union type")

// same function as typeNamesTupleMacro, but for union types
inline def typeNamesUnion[T]: List[String] = ${ typeNamesUnionImpl[T] }
def typeNamesUnionImpl[T: Type](using Quotes): Expr[List[String]] =
  import quotes.reflect.*

  // sanity check
  isUnionSanityCheck[T]

  val t      = TypeRepr.of[T].dealiasKeepOpaques
  val tTypes = get(t)
  Expr(tTypes.map(_.dealiasKeepOpaques.show))

// This weird outer function is necessary to use `TypeRepr` in the signature
def get(using Quotes) =
  import quotes.reflect.*
  def getInner(tr: TypeRepr): List[TypeRepr] =
    tr.dealiasKeepOpaques match
      case OrType(tpes, tpesB) => getInner(tpes) ++ getInner(tpesB)
      case _                   => List(tr)
  getInner
