package typemap

import scala.quoted.*

// A general trait over the backend data structure
// Intented to be implement on different types of HashMap, List, Vector, etc.
// generally, DS = F[V]
trait MutableMapOps[F[_], V]:
  // for compatibility with `MapOps
  def make(length: Int): F[V]
  // TODO can index be using a opaque type in macro?
  def get(ds: F[V], key: String): Option[V]
  def put(ds: F[V], key: String, value: V): Unit
  def computeIfAbsent(ds: F[V], key: String, f: => V): V
  def computeIfPresent(ds: F[V], key: String, f: V => V): Option[V]
  def compute(ds: F[V], key: String, f: Option[V] => V): V

// implement `MapOps` for all types implementing `MutableMapOps`
given [F[_], V](using mutOps: MutableMapOps[F, V]): MapOps[F, V] with
  def make(length: Int): F[V]                                = mutOps.make(length)
  def get(ds: F[V], index: Int, key: String): Option[V]      = mutOps.get(ds, key)
  def put(ds: F[V], index: Int, key: String, value: V): Unit = mutOps.put(ds, key, value)
  def computeIfAbsent(ds: F[V], index: Int, key: String, f: => V): V =
    mutOps.computeIfAbsent(ds, key, f)
  def computeIfPresent(ds: F[V], index: Int, key: String, f: V => V): Option[V] =
    mutOps.computeIfPresent(ds, key, f)
  def compute(ds: F[V], index: Int, key: String, f: Option[V] => V): V =
    mutOps.compute(ds, key, f)

// associate a value of type V to each type in the tuple K
// TODO use PHF
class MutableTypeMap[V, F[_]](private val map: F[V])(using ops: MutableMapOps[F, V]):

  inline def get[T]: Option[V]      = ${ mutableOpImpl[T, V, Option[V]]('{ ops.get(map, _) }) }
  inline def put[T](value: V): Unit = ${ mutableOpImpl[T, V, Unit]('{ ops.put(map, _, value) }) }

def mutableOpImpl[T: Type, V: Type, Result: Type](res: Expr[String => Result])(using
    Quotes
): Expr[Result] =
  '{ ${ res }(${ typeNameImpl[T] }) }

object MutableTypeMap:

  def make[V, F[_]](length: Int)(using ops: MutableMapOps[F, V]): MutableTypeMap[V, F] =
    new MutableTypeMap(ops.make(length))
  def empty[V, F[_]](using ops: MutableMapOps[F, V]): MutableTypeMap[V, F] =
    new MutableTypeMap(ops.make(0))
