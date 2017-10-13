package kodkod

import org.scalatest.{FunSuite, Matchers}

class SpBufTest extends FunSuite with Matchers {
  test("SpBuf#size") {
    SpBuf.empty[Int].size should be (0)
    SpBuf(1, 2, 3).size should be (3)
  }

  test("SpBuf#toString") {
    SpBuf.empty.toString should be ("SpBuf()")
    SpBuf(1, 2, 3).toString should be ("SpBuf(1, 2, 3)")
  }

  test("SpBuf#empty & SpBuf#nonEmpty") {
    val e = SpBuf.empty[Int]
    val ne = SpBuf(1, 2, 3)

    e.nonEmpty should be (None)
    ne.nonEmpty should be (NonEmpty.some)

    e.isEmpty should be (Empty.some)
    ne.isEmpty should be (None)
  }

  test("SpBuf#size & SpBuf#:+ & SpBuf#+:") {
    (1 +: SpBuf.empty[Int]).size should be (1)
    (SpBuf.empty[Int] :+ 1).size should be (1)
    (SpBuf.empty[Int] :+ 1 :+ 3).size should be (2)
    (4 +: SpBuf.empty[Int] :+ 1 :+ 3).size should be (3)

    (1 +: SpBuf(1, 2, 3)).size should be (4)
  }

  test("SpBuf#head & SpBuf#tail") {
    val e = SpBuf.empty[Int]
    val ne = SpBuf(1, 2, 3)

    ne.nonEmpty match {
      case Some(p) =>
        ne.head(p) should be (1)
        ne.last(p) should be (3)
      case None =>
        fail()
    }

    e.nonEmpty match {
      case Some(p) =>
        fail()
      case None => ()
    }
  }
}