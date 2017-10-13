package kodkod

import org.scalatest.{FunSuite, Matchers}

class SliceTest extends FunSuite with Matchers {
  test("Slice#foreach") {
    import UseSideEffects.Implicits.yes

    val slice = Slice(1, 2, 3)
    slice.indices.foreach { i =>
      implicit val p = i.second
      println(slice(i.first))
    }
  }
}
