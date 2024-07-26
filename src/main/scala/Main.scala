import typemap.*

object Main:
  def main(args: Array[String]): Unit =

    println(typeName[Int])
    println(typeName[String])
    println(typeName[Option[Int]])
    println(typeName[List[Option[Int]]])
    println(typeName[List[Double]])
    println(typeNamesTuple[(Int, String, Option[Int])])
