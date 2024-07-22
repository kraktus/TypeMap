import scala.quoted.*

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
private def get(using Quotes) =
  import quotes.reflect.*
  def inner(tr: TypeRepr): List[TypeRepr] =
    tr.dealiasKeepOpaques match
      case OrType(tpes, tpesB) => inner(tpes) ++ inner(tpesB)
      case _                   => List(tr)
  inner
