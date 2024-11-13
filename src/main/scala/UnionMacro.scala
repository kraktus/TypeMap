package typemap

import scala.quoted.*

// same function as isInTuple, but for union types
inline def isInUnion[X, U]: Boolean = ${ isInUnionImpl[X, U] }

def isInUnionImpl[X: Type, U: Type](using Quotes): Expr[Boolean] =
  import quotes.reflect.*
  val xType = TypeRepr.of[X].dealiasKeepOpaques
  Expr(commonImpl[U, Boolean](_.exists(_ =:= xType)))

def unionLengthImpl[U: Type](using Quotes): Expr[Int] =
  Expr(commonImpl[U, Int](_.length))

// for testing only
inline def isUnionCanonical[U]: Unit =
  ${ isUnionCanonicalImpl[U] }

inline def indexInUnion[X, U]: Int = ${ indexInUnionImpl[X, U] }

def indexInUnionImpl[X: Type, U: Type](using Quotes): Expr[Int] =
  import quotes.reflect.*
  val xType = TypeRepr.of[X].dealiasKeepOpaques
  val u     = TypeRepr.of[U].dealiasKeepOpaques
  val f: List[TypeRepr] => Int = uTypes =>
    uTypes.zipWithIndex.find(_._1 =:= xType).map(_._2) match
      case Some(idx) => idx
      case None      => report.errorAndAbort(s"Type ${xType.show} not found in union ${u.show}")
  Expr(commonImpl[U, Int](f))

// same function as typeNamesTupleMacro, but for union types
inline def typeNamesUnion[U]: List[String] = ${ typeNamesUnionImpl[U] }
def typeNamesUnionImpl[U: Type](using q: Quotes): Expr[List[String]] =
  Expr(commonImpl[U, List[String]](_.map(_.dealiasKeepOpaques.show)))

/// ------ internal ------

private def commonImpl[U: Type, Result](using Quotes)(f: List[quotes.reflect.TypeRepr] => Result): Result =
  import quotes.reflect.*
  def listUnionReprInner(tr: TypeRepr): List[TypeRepr] =
    tr.dealiasKeepOpaques match
      case OrType(tpes, tpesB) => listUnionReprInner(tpes) ++ listUnionReprInner(tpesB)
      case _                   => List(tr)

  // sanity check
  isUnionCanonicalImpl[U]
  val u      = TypeRepr.of[U].dealiasKeepOpaques
  val uTypes = listUnionReprInner(u)
  f(uTypes)

private def isUnionCanonicalImpl[U: Type](using Quotes): Expr[Unit] =
  import quotes.reflect.*
  isUnionSanityCheck[U]
  val u = TypeRepr.of[U].dealiasKeepOpaques

  def inner[U: Type](s: Set[TypeRepr], tr: TypeRepr): Set[TypeRepr] =
    tr.dealiasKeepOpaques match
      case OrType(a, b) =>
        inner[U](inner[U](s, a), b)
      case x if s.contains(x) =>
        report.errorAndAbort(s"Type ${x.show} found multiple times (CHECK ALIASES) in union ${u.show}")
      case x => s + x

  inner(Set.empty, u)
  '{ () }

private def isUnionSanityCheck[T: Type](using Quotes): Unit =
  import quotes.reflect.*
  val t = TypeRepr.of[T].dealiasKeepOpaques
  t match
    case OrType(_, _) => ()
    case x            => report.errorAndAbort(s"Type ${x.show} is not a union type")
