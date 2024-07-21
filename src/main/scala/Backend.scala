import java.util.concurrent.ConcurrentHashMap as CMap
import scala.collection.mutable.ArrayBuffer

type CMapBackend = [X] =>> CMap[String, X]
given [Value]: BackendOps[CMapBackend, Value] with
  private type DS = CMap[String, Value]
  def empty: DS                                                = new DS()
  def get(ds: DS, index: Int, key: String): Option[Value]      = Option(ds.get(key))
  def put(ds: DS, index: Int, key: String, value: Value): Unit = ds.put(key, value)

type ArrayBufferBackend = [X] =>> ArrayBuffer[X]
given [Value]: BackendOps[ArrayBufferBackend, Value] with
  private type DS = ArrayBuffer[Value]
  def empty: DS                                                = ArrayBuffer.empty
  def get(ds: DS, index: Int, key: String): Option[Value]      = ds.lift(index)
  def put(ds: DS, index: Int, key: String, value: Value): Unit = ds(index) = value
