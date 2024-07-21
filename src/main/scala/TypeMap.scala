import scala.quoted.*

// A general trait over the backend data structure
// Intented to be implement on different types of HashMap, List, Vector, etc.
// generally, DS = F[V]
trait BackendOps[F[_], V]:
  def empty: F[V]
  // FIXME, can these functions be inlined?
  // otherwise got `Deferred inline method get in trait BackendOps cannot be invoked
  def get(ds: F[V], keys: List[String], key: String): Option[V]
  def put(ds: F[V], keys: List[String], key: String, value: V): Unit

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap[K, V, F[_]](private val map: F[V])(using bops: BackendOps[F, V]):

  inline def get[T]: Option[V]      = ${ opImpl[T, K, V, Option[V]]('{ bops.get(map, _, _) }) }
  inline def put[T](value: V): Unit = ${ opImpl[T, K, V, Unit]('{ bops.put(map, _, _, value) }) }

def opImpl[T: Type, K: Type, V: Type, Result: Type](res: Expr[(List[String], String) => Result])(using
    Quotes
): Expr[Result] =
  import quotes.reflect.report
  isInUnionImpl[T, K] match
    case Expr(true)  => '{ ${ res }(${ typeNamesUnionImpl[K] }, ${ typeNameImpl[T] }) }
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")

object TypeMap:
  def empty[K, V, F[_]](using ds: BackendOps[F, V]) = TypeMap[K, V, F](ds.empty)
