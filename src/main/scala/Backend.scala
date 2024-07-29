package typemap

import java.util.concurrent.ConcurrentHashMap as CMap
import java.util.concurrent.CopyOnWriteArrayList as CowArray
import scala.collection.mutable.ArraySeq

type CMapBackend = [X] =>> CMap[String, X]
given [Value]: MutableMapOps[CMapBackend, Value] with
  private type DS = CMapBackend[Value]
  def make(length: Int): DS                        = new DS(length)
  def get(ds: DS, key: String): Option[Value]      = Option(ds.get(key))
  def put(ds: DS, key: String, value: Value): Unit = ds.put(key, value)

type ArraySeqBackend = [X] =>> ArraySeq[Option[X]]
given [Value]: MapOps[ArraySeqBackend, Value] with
  private type DS = ArraySeqBackend[Value]
  def make(length: Int): DS                                    = ArraySeq.fill(length)(None)
  def get(ds: DS, index: Int, key: String): Option[Value]      = ds.lift(index).flatten
  def put(ds: DS, index: Int, key: String, value: Value): Unit = ds(index) = Some(value)

type CowArrayBackend = [X] =>> CowArray[Option[X]]
given [Value]: MapOps[CowArrayBackend, Value] with
  private type DS = CowArrayBackend[Value]
  def make(length: Int): DS =
    val array: Array[Option[Value]] = ArraySeq.fill(length)(None).toArray
    new CowArray(array)
  def get(ds: DS, index: Int, key: String): Option[Value]      = Option(ds.get(index)).flatten
  def put(ds: DS, index: Int, key: String, value: Value): Unit = ds.add(index, Some(value))

// TODO, implement for more backends (including concurrent), to benchmark them
// https://stackoverflow.com/questions/28128786/how-to-use-concurrentlinkedqueue-in-scala
