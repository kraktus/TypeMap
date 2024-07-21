import java.util.concurrent.ConcurrentHashMap as CMap
import scala.quoted.*

type Backend[V] = CMap[String, V]

// DS = Data Structure
trait BackendOps[DS, V]:
  extension (ds: DS)
    def get(key: String): Option[V]
    def put(key: String, value: V): Unit

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap[K, V](private val map: Backend[V]):

  inline def get[T]: Option[V]      = ${ getImpl[T, K, V]('{x => Option(map.get(x))}) }
  inline def put[T](value: V): Unit = ${ putImpl[T, K, V]('map, 'value) }

def getImpl[T: Type, K: Type, V: Type](
    res: Expr[String => Option[V]],
)(using Quotes): Expr[Option[V]] =
  opImpl[T, K, V, Option[V]]('{ ${res}(${ typeNameImpl[T] }) })

def putImpl[T: Type, K: Type, V: Type](map: Expr[Backend[V]], value: Expr[V])(using
    Quotes
): Expr[Unit] =
  opImpl[T, K, V, Unit]('{ ${ map }.put(${ typeNameImpl[T] }, ${ value }) })

def opImpl[T: Type, K: Type, V: Type, Result](res: Expr[Result])(using
    Quotes
): Expr[Result] =
  import quotes.reflect.report
  isInUnionImpl[T, K] match
    case Expr(true)  => res
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")

object TypeMap:
  def empty[K, V] = TypeMap[K, V](new Backend())
