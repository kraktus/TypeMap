## Scala3 TypeMap

This project is a simple implementation of a type-level map in Scala3, using macros. Inspired by the rust crate [`anymap`](https://docs.rs/anymap/latest/anymap/).

### INSTALL

Clone typemap

```
$ git clone https://github.com/lichess-org/TypeMap
```

Start [sbt](http://www.scala-sbt.org/download.html) in typemap directory

```
$ sbt
```

In the sbt shell, to compile typemap and examples, run

```
sbt:typemap> build
```

To run the tests

```
sbt:typemap> test
```

To compile or test the examples

```
sbt:typemap> examples / compile (or test)
```

To run benchmarks:

```
sbt:typemap> bench / Jmh / run
```

Or to ouput a json file

```
sbt:typemap> bench / Jmh / run -rf json
```

To run quick benchmarks (results may be inaccurate):

```
sbt:typemap> bench / Jmh / run -i 1 -wi 1 -f1 -t1
```

To run benchmarks for a specific class:

```
sbt:typemap> bench / Jmh / run -rf json .*PlayBench.*
```

To run [scalafmt](https://scalameta.org/scalafmt/docs/installation.html):

```
sbt:typemap> fmt
```

To run sbt-coverage

```
$ ./coverage.sh
```

````

### Usage

```scala
val map: TypeMap[Int | String, String] = TypeMap.empty
map.put[Int]("1")
assertEquals(map.get[Int], Some("1"))
assertEquals(map.get[String], None)
// does not compile, as expected
map.get[Float]
````

