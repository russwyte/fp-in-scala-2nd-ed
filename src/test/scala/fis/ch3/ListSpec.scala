package fis.ch3
import List._

class ListSpec extends munit.FunSuite:
  test("constructors") {
    val check = Cons(1, (Cons(2, Nil)))
    assert(1 :: 2 :: Nil == check)
    assert(List(1, 2) == check)
  }

  test("foldLeft") {
    val res = List(1, 2).foldLeft(0) { (b, a) => b + a }
    assert(res == 3)
  }

  test("reverse") {
    assert(List(1, 2).reverse == List(2, 1))
  }

  test("foldRight") {
    assert(List(1, 2).foldRight("") { (a, b) => b + a } == "21")
  }

  test("++") {
    assert(List(1, 2) ++ List(3, 4) == List(1, 2, 3, 4))
  }

  test("map") {
    assert(List(1, 2).map(_ * 2) == List(2, 4))
  }

  test("flatten") {
    assert(List(List(1, 2), List(3, 4)).flatten == List(1, 2, 3, 4))
  }

  test("flatMap") {
    assert(List(1, 2).flatMap(x => List(x, x * 2)) == List(1, 2, 2, 4))
  }

  test("toString") {
    assert(List(1, 2).toString == "List(1,2)")
  }

  test("sum") {
    assert(List(1, 2).sum == 3)
  }

  test("product") {
    assert(List(1, 2).product == 2)
  }

  test("headOption") {
    assert {
      List(1, 2).headOption.exists(_ == 1)
    }
  }

  test("tailOption") {
    assert {
      List(1, 2).tailOption.exists(_ == List(2)) && Nil.tailOption.isEmpty
    }
  }

  test("initOption") {
    assert {
      List(1, 2).initOption.exists(_ == List(1)) && Nil.initOption.isEmpty
    }
  }

  test("length") {
    assert {
      List().length == 0 && Nil.length == 0
    }
    assert {
      List(1, 2, 3).length == 3
    }
  }

  test("filter") {
    assert {
      List(1, 2, 3, 4).filter((_ < 3)) == List(1, 2)
    }
  }

  test("zip") {
    assert {
      List(1, 2, 3).zip(List(4, 5, 6)) == List((1, 4), (2, 5), (3, 6))
      List(1, 2, 3).zip(List(4, 5, 6, 7)) == List((1, 4), (2, 5), (3, 6))
      List(1, 2, 3, 4).zip(List(4, 5, 6)) == List((1, 4), (2, 5), (3, 6))
    }
  }

  test("combine") {
    assert {
      List(1, 2, 3).combine(List("a", "b", "c")) { (a, b) =>
        s"$a:$b"
      } == (List("1:a", "2:b", "3:c"))
    }
  }

  test("take") {
    assert {
      List(1, 2, 3).take(2) == List(1, 2)
    }
  }

  test("startsWith") {
    assert {
      List(1, 2, 3).startsWith(List(1, 2)) && !List(2, 3).startsWith(
        List(1, 2)
      )
    }
  }

  test("hasSubsequence") {
    val l = List(1, 2, 3, 4)
    assert {
      l.hasSubsequence(List(1, 2)) &&
      l.hasSubsequence(List(2, 3)) &&
      l.hasSubsequence(List(2, 3, 4)) &&
      l.hasSubsequence(List(4)) &&
      l.hasSubsequence(Nil)
    }
    assert {
      !l.hasSubsequence(List(0, 1)) &&
      !l.hasSubsequence(List(1, 2, 3, 4, 5))
    }
  }

  test("drop") {
    assert {
      List(1, 2, 3, 4).drop(2) == List(3, 4)
    }
  }

  test("head") {
    assert(List(1, 2, 3).head == 1)
  }

  test("tail") {
    assert(List(1, 2, 3).tail == List(2, 3))
  }

  test("reduce") {
    assert(List(1, 2, 3).reduce(_ + _) == 6)
  }
