package kodkod

import org.scalatest.{FunSuite, Matchers}

class BufTest extends FunSuite with Matchers {
    test("Buf#size") {
        Buf.empty.size should be (0)
        Buf(1, 2, 3).size should be (3)
    }

    test("Buf#toString") {
        Buf.empty.toString should be ("Buf()")
        Buf(1, 2, 3).toString should be ("Buf(1, 2, 3)")
    }

    test("Buf#empty & Buf#nonEmpty") {
        val e = Buf.empty[Int]
        val ne = Buf(1, 2, 3)

        e.nonEmpty should be (None)
        ne.nonEmpty should be (NonEmpty.some)

        e.isEmpty should be (Empty.some)
        ne.isEmpty should be (None)
    }

    test("Buf#size & Buf#:+ & Buf#+:") {
        (1 +: Buf.empty).size should be (1)
        (Buf.empty :+ 1).size should be (1)
        (Buf.empty :+ 1 :+ 3).size should be (2)
        (4 +: Buf.empty :+ 1 :+ 3).size should be (3)

        (1 +: Buf(1, 2, 3)).size should be (4)
    }

    test("Buf#head & Buf#tail") {
        val e = Buf.empty[Int]
        val ne = Buf(1, 2, 3)

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
