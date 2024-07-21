import java.util.concurrent.ConcurrentHashMap as CMap
import scala.collection.mutable.ArrayBuffer

type CMapBackend = [X] =>> CMap[String, X]
given [Value]: BackendOps[CMapBackend, Value] with
  private type DS = CMap[String, Value]
  def empty: DS                                                      = new DS()
  def get(ds: DS, _l: List[String], key: String): Option[Value]      = Option(ds.get(key))
  def put(ds: DS, _l: List[String], key: String, value: Value): Unit = ds.put(key, value)

def findWhere(s: Seq[String], key: String): Option[Int] = s.zipWithIndex.find(_._1 == key).map(_._2)

type ArrayBufferBackend = [X] =>> ArrayBuffer[X]
given [Value]: BackendOps[ArrayBufferBackend, Value] with
  private type DS = ArrayBuffer[Value]
  def empty: DS                                                     = ArrayBuffer.empty
  def get(ds: DS, l: List[String], key: String): Option[Value]      = findWhere(l, key).map(ds(_))
  def put(ds: DS, l: List[String], key: String, value: Value): Unit = findWhere(l, key).foreach(ds(_) = value)
