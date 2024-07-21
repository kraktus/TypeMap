import java.util.concurrent.ConcurrentHashMap as CMap
import scala.quoted.*

type Backend2[V] = CMap[String, V]

trait BackendOps4[DS, V]:
  def empty: DS
  // TODO, is is possible to use extension method that works in macro context?
  inline def get(ds: DS, key: String): Option[V]
  inline def put(ds: DS, key: String, value: V): Unit

given [Value]: BackendOps4[CMap[String, Value], Value] with
  def empty: CMap[String, Value]                                           = new CMap[String, Value]()
  inline def get(ds: CMap[String, Value], key: String): Option[Value]      = Option(ds.get(key))
  inline def put(ds: CMap[String, Value], key: String, value: Value): Unit = ds.put(key, value)

// DS = Data Structure
trait BackendOps2[DS, V]:
  extension (ds: DS)
    def get(key: String): Option[V]
    def put(key: String, value: V): Unit

// associate a value of type V to each type in the tuple K
// TODO use PHF
class TypeMap2[K, V, DS](private val map: DS)(using bops: BackendOps4[DS, V]):
  inline def get[T]: Option[V] = ${ getImpl2[T, K, V, DS]('{ bops.get(map, _) }) }

def getImpl2[T: Type, K: Type, V: Type, DS](res: Expr[String => Option[V]])(using
    Quotes
): Expr[Option[V]] =
  import quotes.reflect.report
  isInUnionImpl[T, K] match
    case Expr(true)  => '{ ${res}(${ typeNameImpl[T] }) }
    case Expr(false) => report.errorAndAbort(s"Type ${Type.show[T]} not found in tuple ${Type.show[K]}")

object TypeMap2:
  def empty[K, V, DS](using ds: BackendOps4[DS, V]) = TypeMap2[K, V, DS](ds.empty)
