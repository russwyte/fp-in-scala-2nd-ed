package fis.ch3
import Tree._
class TreeSpec extends munit.FunSuite:
  val T1 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  test("fold") {
    assert {
      T1.fold(identity)(_ + _) == 10
    }
  }
  test("size") {
    assert {
      T1.size == 4
    }
  }
