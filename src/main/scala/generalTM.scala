import java.util.concurrent.ConcurrentHashMap as CMap
import scala.quoted.*

type Backend2[V] = CMap[String, V]

trait BackendOps4[DS, V]:
  def empty: DS
  // TODO, is is possible to use extension method that works in macro context?
  inline def get(ds: DS, key: String): Option[V]
  inline def put(ds: DS, key: String, value: V): Unit

// DS = F[V]
trait BackendOps5[F[_], V]:
  def empty: F[V]
  // TODO, is is possible to use extension method that works in macro context?
  inline def get(ds: F[V], key: String): Option[V]
  inline def put(ds: F[V], key: String, value: V): Unit

given [Value]: BackendOps4[CMap[String, Value], Value] with
  def empty: CMap[String, Value]                                           = new CMap[String, Value]()
  inline def get(ds: CMap[String, Value], key: String): Option[Value]      = Option(ds.get(key))
  inline def put(ds: CMap[String, Value], key: String, value: Value): Unit = ds.put(key, value)

given [Value]: BackendOps5[[X] =>> CMap[String, X], Value] with
  private type DS = CMap[String, Value]
  def empty: DS                                           = new DS()
  inline def get(ds: DS, key: String): Option[Value]      = Option(ds.get(key))
  inline def put(ds: DS, key: String, value: Value): Unit = ds.put(key, value)

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap2[K, V, F[_]](private val map: F[V])(using bops: BackendOps5[F, V]):
  inline def get[T]: Option[V] = ${ getImpl2[T, K, V]('{ bops.get(map, _) }) }

def getImpl2[T: Type, K: Type, V: Type](res: Expr[String => Option[V]])(using
    Quotes
): Expr[Option[V]] =
  import quotes.reflect.report
  isInUnionImpl[T, K] match
    case Expr(true)  => '{ ${ res }(${ typeNameImpl[T] }) }
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")

object TypeMap2:
  def empty[K, V, F[_]](using ds: BackendOps5[F, V]) = TypeMap2[K, V, F](ds.empty)
