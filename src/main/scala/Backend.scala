package typemap

import java.util.concurrent.ConcurrentHashMap as CMap
import scala.collection.mutable.ArrayBuffer

type CMapBackend = [X] =>> CMap[String, X]
given [Value]: MapOps[CMapBackend, Value] with
  private type DS = CMapBackend[Value]
  def make(length: Int): DS                                    = new DS(length)
  def get(ds: DS, index: Int, key: String): Option[Value]      = Option(ds.get(key))
  def put(ds: DS, index: Int, key: String, value: Value): Unit = ds.put(key, value)

type ArrayBufferBackend = [X] =>> ArrayBuffer[Option[X]]
given [Value]: MapOps[ArrayBufferBackend, Value] with
  private type DS = ArrayBufferBackend[Value]
  def make(length: Int): DS                                    = ArrayBuffer.fill(length)(None)
  def get(ds: DS, index: Int, key: String): Option[Value]      = ds.lift(index).flatten
  def put(ds: DS, index: Int, key: String, value: Value): Unit = ds(index) = Some(value)

// TODO, implement for more backends (including concurrent), to benchmark them
// https://stackoverflow.com/questions/28128786/how-to-use-concurrentlinkedqueue-in-scala
