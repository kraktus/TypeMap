package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import typemap.{ TypeMap, CMapBackend, ArraySeqBackend, CowArrayBackend, given }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 1)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 1)
@Fork(value = 1)
@Threads(value = 1)
class TypeMapBenchmark:
  private type Keys = Int | String | Float

  @Param(Array("cmap", "arraySeq", "cowArray"))
  @annotation.nowarn
  private var backend: String = ""

  var tmap: TypeMap[Keys, String, ?] = scala.compiletime.uninitialized

  @Setup
  def setup() =
    tmap = backend match
      case "cmap"     => TypeMap.empty[Keys, String, CMapBackend]
      case "arraySeq" => TypeMap.empty[Keys, String, ArraySeqBackend]
      case "cowArray" => TypeMap.empty[Keys, String, CowArrayBackend]
      case _          => ???

    tmap.put[Int]("1")
    tmap.put[String]("2")

  @Benchmark
  def mapGet(bh: Blackhole) =
    bh.consume(tmap.get[Int])

  @Benchmark
  def mapGetEmpty(bh: Blackhole) =
    bh.consume(tmap.get[Float])

  @Benchmark
  def mapPut(bh: Blackhole) =
    bh.consume(tmap.put[Float]("3"))
