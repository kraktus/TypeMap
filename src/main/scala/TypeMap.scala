import scala.quoted.*

// A general trait over the backend data structure
// Intented to be implement on different types of HashMap, List, Vector, etc.
// generally, DS = F[V]
trait BackendOps[F[_], V]:
  def empty: F[V]
  def make(length: Int): F[V]
  // depending on the underlying backend, the index or key will be used
  // index of the type `T` in the type union `K` of `TypeMap`
  // TODO can index be using a opaque int in macro?
  def get(ds: F[V], index: Int, key: String): Option[V]
  def put(ds: F[V], index: Int, key: String, value: V): Unit

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap[K, V, F[_]](private val map: F[V])(using bops: BackendOps[F, V]):

  inline def get[T]: Option[V]      = ${ opImpl[T, K, V, Option[V]]('{ bops.get(map, _, _) }) }
  inline def put[T](value: V): Unit = ${ opImpl[T, K, V, Unit]('{ bops.put(map, _, _, value) }) }

def opImpl[T: Type, K: Type, V: Type, Result: Type](res: Expr[(Int, String) => Result])(using
    Quotes
): Expr[Result] =
  import quotes.reflect.report
  isInUnionImpl[T, K] match
    case Expr(true)  => '{ ${ res }(${ indexInUnionImpl[T, K] }, ${ typeNameImpl[T] }) }
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")

object TypeMap:
  inline def empty[K, V, F[_]](using ds: BackendOps[F, V]): TypeMap[K, V, F] = ${ emptyImpl[K, V, F]('ds) }
  def emptyImpl[K: Type, V: Type, F[_]: Type](ds: Expr[BackendOps[F, V]])(using
      Quotes
  ): Expr[TypeMap[K, V, F]] =
    '{ TypeMap[K, V, F](${ ds }.make(${ unionLengthImpl[K] }))(using ${ ds }) }
