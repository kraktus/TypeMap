import java.util.concurrent.ConcurrentHashMap as CMap
import scala.quoted.*

type Backend[V] = CMap[String, V]

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap[K <: Tuple, V](private val map: Backend[V]):

  inline def get[T]: Option[V]      = ${ getImpl[T, K, V]('map) }
  inline def put[T](value: V): Unit = ${ putImpl[T, K, V]('map, 'value) }

def getImpl[T, K <: Tuple, V](
    map: Expr[CMap[String, V]]
)(using Quotes, Type[T], Type[K], Type[V]): Expr[Option[V]] =
  opImpl[T, K, V, Option[V]](map, '{ Option(${ map }.get(${ typeNameImpl[T] })) })

def putImpl[T, K <: Tuple, V](map: Expr[Backend[V]], value: Expr[V])(using
    Quotes,
    Type[T],
    Type[K],
    Type[V]
): Expr[Unit] =
  opImpl[T, K, V, Unit](map, '{ ${ map }.put(${ typeNameImpl[T] }, ${ value }) })

def opImpl[T, K <: Tuple, V, Result](map: Expr[Backend[V]], res: Expr[Result])(using
    Quotes,
    Type[T],
    Type[K],
    Type[V]
): Expr[Result] =
  import quotes.reflect.report
  isInTupleImpl[T, K] match
    case Expr(true)  => res
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")

object TypeMap:
  def empty[K <: Tuple, V] = TypeMap[K, V](new Backend())
