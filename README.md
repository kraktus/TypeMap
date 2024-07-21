## Scala3 TypeMap

This project is a simple implementation of a type-level map in Scala3, using macros. Inspired by the rust crate [`anymap`](https://docs.rs/anymap/latest/anymap/).

### Usage

```scala
    val map: TypeMap[Int | String, String] = TypeMap.empty
    map.put[Int]("1")
    assertEquals(map.get[Int], Some("1"))
    assertEquals(map.get[String], None)
    
    // does not compile, as expected
    // assertEquals(map.get[Float], None)
```