import java.util.concurrent.ConcurrentHashMap as CMap

// DS = Data Structure
// trait BackendOps2[DS, V]:
//   extension(ds: DS)
//     def empty: DS
//     inline def get(key: String): Option[V]
//     inline def put(key: String, value: V): Unit

// given [V](BackendOps2[CMap[String, V], V]) with
//   extension (ds: CMap[String, V])
//     inline def empty: CMap[String, V] = new CMap[String, V]()
//     inline def get(key: String): Option[V] = Option(ds.get(key))
//     inline def put(key: String, value: V): Unit = ds.put(key, value)

trait BackendOps3:
  type V;
  def empty: this.type
  inline def get(key: String): Option[V]
  inline def put(key: String, value: V): Unit

// CMap = ConcurrentHashMap
class MyClass[Value](val map: CMap[String, Value]) extends BackendOps3:
  type V = Value
  def empty                                   = this
  inline def get(key: String): Option[V]      = Option(map.get(key))
  inline def put(key: String, value: V): Unit = map.put(key, value)

trait BackendOps4[DS, V]:
  def empty: DS
  extension (ds: DS)
    inline def get(key: String): Option[V]
    inline def put(key: String, value: V): Unit

given foo[Value]: BackendOps4[CMap[String, Value], Value] with
  def empty: CMap[String, Value] = new CMap[String, Value]()
  extension (ds: CMap[String, Value])
    inline def get(key: String): Option[Value]      = Option(ds.get(key))
    inline def put(key: String, value: Value): Unit = ds.put(key, value)

trait BackendOps5[F[_], V]:
  def empty: F[V]
  extension (x: F[V])
    inline def get(key: String): Option[V]
    inline def put(key: String, value: V): Unit

trait BackendOps6[F[_]]:
  type V;
  def empty: F[V]
  extension (x: F[V])
    inline def get(key: String): Option[V]
    inline def put(key: String, value: V): Unit

// associates a value of type V to each type in the tuple K
class TypeMap2[K, V, DS](private val map: DS)(using BackendOps4[DS, V]):
  inline def getf = map.get

object TypeMap2:
  def empty[K, V, DS](using ds: BackendOps4[DS, V]) = TypeMap2[K, V, DS](ds.empty)

// associates a value of type V to each type in the tuple K
class TypeMap3[K, V, DS[_]](private val map: DS[V])(using BackendOps5[DS, V]):
  inline def getf = map.get

object TypeMap3:
  def empty[K, V, DS[_]](using ds: BackendOps5[DS, V]) = TypeMap3[K, V, DS](ds.empty)
