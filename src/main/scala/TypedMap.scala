import java.util.concurrent.ConcurrentHashMap as CMap
import scala.quoted.*

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap[K <: Tuple, V](private val map: CMap[String, V]):
  inline def keys = typeNamesTupleMacro[K] // Obviously not known here because K is generic

  inline def get[T]: Option[V] = ${ getImpl[T, K, V]('map) }
  inline def put[T](value: V): Unit = ${ putImpl[T, K, V]('map, 'value) }


def getImpl[T, K <: Tuple, V](map: Expr[CMap[String, V]])(using Quotes, Type[T], Type[K], Type[V]): Expr[Option[V]] =
  // import quotes.reflect.report
  // val tNameExpr = Expr(Type.show[T])
  // isInTupleImpl[T, K] match
  //   case Expr(true) => '{ Option(${ map }.get(${ tNameExpr })) }
  //   case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")
  opImpl[T, K, V, Option[V]](map, '{ Option(${ map }.get(${ Expr(Type.show[T]) })) })

def putImpl[T, K <: Tuple, V](map: Expr[CMap[String, V]], value: Expr[V])(using Quotes, Type[T], Type[K], Type[V]): Expr[Unit] =
  import quotes.reflect.report
  val tNameExpr = Expr(Type.show[T])
  isInTupleImpl[T, K] match
    case Expr(true) => '{ ${ map }.put(${ tNameExpr }, ${ value }) }
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")


def opImpl[T, K <: Tuple, V, Result](map: Expr[CMap[String, V]], res: Expr[Result])(using Quotes, Type[T], Type[K], Type[V]): Expr[Result] =
  import quotes.reflect.report
  isInTupleImpl[T, K] match
    case Expr(true) => res
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")


object TypeMap:
  def empty[K <: Tuple, V] = TypeMap[K, V](new CMap())