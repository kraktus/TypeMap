package typemap

import java.util.concurrent.ConcurrentHashMap as CMap
import java.util.concurrent.CopyOnWriteArrayList as CowArray
import scala.collection.mutable.ArraySeq

type CMapBackend = [X] =>> CMap[String, X]
given [V] => MutableMapOps[CMapBackend, V]:
  private type DS = CMapBackend[V]
  def make(length: Int): DS                    = new DS(length)
  def get(ds: DS, key: String): Option[V]      = Option(ds.get(key))
  def put(ds: DS, key: String, value: V): Unit = ds.put(key, value)

given [V] => ThreadSafeMutableMapOps[CMapBackend, V]:
  private type DS = CMapBackend[V]
  def computeIfAbsent(ds: DS, key: String, f: => Option[V]): Option[V] =
    Option:
      ds.computeIfAbsent(key, _ => f.getOrElse(null.asInstanceOf[V]))
  def computeIfPresent(ds: DS, key: String, f: V => Option[V]): Option[V] =
    Option:
      ds.computeIfPresent(key, (_, v) => f(v).getOrElse(null.asInstanceOf[V]))
  def compute(ds: DS, key: String, f: Option[V] => Option[V]): Option[V] =
    Option:
      ds.compute(key, (_, v) => f(Option(v)).getOrElse(null.asInstanceOf[V]))

type ArraySeqBackend = [X] =>> ArraySeq[Option[X]]
given [V] => MapOps[ArraySeqBackend, V]:
  private type DS = ArraySeqBackend[V]
  def make(length: Int): DS                                = ArraySeq.fill(length)(None)
  def get(ds: DS, index: Int, key: String): Option[V]      = ds.lift(index).flatten
  def put(ds: DS, index: Int, key: String, value: V): Unit = ds(index) = Some(value)

type CowArrayBackend = [X] =>> CowArray[Option[X]]
given [V] => MapOps[CowArrayBackend, V]:
  private type DS = CowArrayBackend[V]
  def make(length: Int): DS =
    val array: Array[Option[V]] = ArraySeq.fill(length)(None).toArray
    new CowArray(array)
  def get(ds: DS, index: Int, key: String): Option[V]      = Option(ds.get(index)).flatten
  def put(ds: DS, index: Int, key: String, value: V): Unit = ds.add(index, Some(value))
// TODO, implement for more backends (including concurrent), to benchmark them
// https://stackoverflow.com/questions/28128786/how-to-use-concurrentlinkedqueue-in-scala
