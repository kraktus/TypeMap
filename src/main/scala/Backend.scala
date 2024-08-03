package typemap

import java.util.concurrent.ConcurrentHashMap as CMap
import java.util.concurrent.CopyOnWriteArrayList as CowArray
import scala.collection.mutable.ArraySeq

type CMapBackend = [X] =>> CMap[String, X]
given [V]: MutableMapOps[CMapBackend, V] with
  private type DS = CMapBackend[V]
  def make(length: Int): DS                                          = new DS(length)
  def get(ds: DS, key: String): Option[V]                            = Option(ds.get(key))
  def put(ds: DS, key: String, value: V): Unit                       = ds.put(key, value)
  def computeIfAbsent(ds: DS, key: String, f: => V): V = ds.computeIfAbsent(key, _ => f)
  def computeIfPresent(ds: DS, key: String, f: V => V): Option[V] = 
    Option(ds.computeIfPresent(key, (k, v) => f(v)))
  def compute(ds: DS, key: String, f: Option[V] => V): V =
    ds.compute(key, (k, v) => f(Option(v)))

type ArraySeqBackend = [X] =>> ArraySeq[Option[X]]
given [V]: MapOps[ArraySeqBackend, V] with
  private type DS = ArraySeqBackend[V]
  def make(length: Int): DS                                = ArraySeq.fill(length)(None)
  def get(ds: DS, index: Int, key: String): Option[V]      = ds.lift(index).flatten
  def put(ds: DS, index: Int, key: String, value: V): Unit = ds(index) = Some(value)
  def computeIfAbsent(ds: DS, index: Int, key: String, f: => V): V = 
    val v = get(ds, index, key).getOrElse(f)
    put(ds, index, key, v)
    v
  def computeIfPresent(ds: DS, index: Int, key: String, f: V => V): Option[V] =
    get(ds, index, key).map(f)
  def compute(ds: DS, index: Int, key: String, f: Option[V] => V): V =
    val v = f(get(ds, index, key))
    put(ds, index, key, v)
    v



type CowArrayBackend = [X] =>> CowArray[Option[X]]
given [V]: MapOps[CowArrayBackend, V] with
  private type DS = CowArrayBackend[V]
  def make(length: Int): DS =
    val array: Array[Option[V]] = ArraySeq.fill(length)(None).toArray
    new CowArray(array)
  def get(ds: DS, index: Int, key: String): Option[V]      = Option(ds.get(index)).flatten
  def put(ds: DS, index: Int, key: String, value: V): Unit = ds.add(index, Some(value))
  def computeIfAbsent(ds: DS, index: Int, key: String, f: => V): V = 
    val v = get(ds, index, key).getOrElse(f)
    put(ds, index, key, v)
    v
  def computeIfPresent(ds: DS, index: Int, key: String, f: V => V): Option[V] =
    get(ds, index, key).map(f)
  def compute(ds: DS, index: Int, key: String, f: Option[V] => V): V =
    val v = f(get(ds, index, key))
    put(ds, index, key, v)
    v
// TODO, implement for more backends (including concurrent), to benchmark them
// https://stackoverflow.com/questions/28128786/how-to-use-concurrentlinkedqueue-in-scala
