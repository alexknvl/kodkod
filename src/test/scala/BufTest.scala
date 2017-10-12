package kodkod

import org.scalatest.{FunSuite, Matchers}

class BufTest extends FunSuite with Matchers {
    test("buf size") {
        Buf.empty.size should be (0)
        Buf(1, 2, 3).size should be (3)
    }

    test("size & cons / snoc") {
        (1 +: Buf.empty).size should be (1)
        (Buf.empty :+ 1).size should be (1)
        (Buf.empty :+ 1 :+ 3).size should be (2)
        (4 +: Buf.empty :+ 1 :+ 3).size should be (3)

        (1 +: Buf(1, 2, 3)).size should be (4)
    }

    test("buf head & tail") {
        val e = Buf.empty[Int]
        val ne = Buf(1, 2, 3)

        e.nonEmpty should be (None)
        ne.nonEmpty should be (NonEmpty.some)

        ne.nonEmpty match {
            case Some(p) =>
                ne.head(p) should be (1)
                ne.last(p) should be (3)
            case None =>
                fail()
        }
    }
}
