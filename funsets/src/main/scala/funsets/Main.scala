package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s12 = union(s1, s2)
  val s123 = union(s12,s3)
  val s23 = union(s2, s3)
  val mapRes = map(s123, (x) => x * x);
  printSet(s123)

  printSet(mapRes)

  println(exists(s123, (y: Int) => y*y == 4))
}
