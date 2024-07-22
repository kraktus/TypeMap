import scala.quoted.*

// same function as isInTuple, but for union types
inline def isInUnion[X, U]: Boolean = ${ isInUnionImpl[X, U] }

private def isInUnionImpl[X: Type, U: Type](using Quotes): Expr[Boolean] =
  import quotes.reflect.*
  // sanity check
  isUnionSanityCheck[U]

  val xType  = TypeRepr.of[X].dealiasKeepOpaques
  val u      = TypeRepr.of[U].dealiasKeepOpaques
  val uTypes = listUnionRepr(u)
  Expr(uTypes.exists(_ =:= xType))

def unionLengthImpl[U: Type](using Quotes): Expr[Int] =
  import quotes.reflect.*
  // sanity check
  isUnionSanityCheck[U]

  val uTypes = listUnionRepr(TypeRepr.of[U].dealiasKeepOpaques)
  Expr(uTypes.length)

inline def indexInUnion[X, U]: Int = ${ indexInUnionImpl[X, U] }

private def indexInUnionImpl[X: Type, U: Type](using Quotes): Expr[Int] =
  import quotes.reflect.*
  // sanity check
  isUnionSanityCheck[U]

  val xType  = TypeRepr.of[X].dealiasKeepOpaques
  val u      = TypeRepr.of[U].dealiasKeepOpaques
  val uTypes = listUnionRepr(u)
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
inline def typeNamesUnion[U]: List[String] = ${ typeNamesUnionImpl[U] }
def typeNamesUnionImpl[U: Type](using q: Quotes): Expr[List[String]] =
  import quotes.reflect.*
  val f: List[quotes.reflect.TypeRepr] => List[String] = x => x.map(_.dealiasKeepOpaques.show)
  Expr(commonImpl[U, List[String]](_.map(_.dealiasKeepOpaques.show)))


private def commonImpl[U: Type, Result](using Quotes)(f: List[quotes.reflect.TypeRepr] => Result): Result =
  import quotes.reflect.*
  def listUnionReprInner(tr: TypeRepr): List[TypeRepr] =
    tr.dealiasKeepOpaques match
      case OrType(tpes, tpesB) => listUnionReprInner(tpes) ++ listUnionReprInner(tpesB)
      case _                   => List(tr)

  // sanity check
  isUnionSanityCheck[U]
  val u = TypeRepr.of[U].dealiasKeepOpaques
  val uTypes = listUnionReprInner(u)
  f(uTypes)

