package fis.ch3
import List._

class ListSpec extends munit.FunSuite:
  val l = List(1, 2)
  test("cons `::`") {
    val res = 1 :: 2 :: Nil
    assert(res == List.Cons(1, Cons(2, Nil)))
    assert(res == l)
  }
  test("foldLeft") {
    val res = l.foldLeft(0) { (b, a) => b + a }
    assert(res == 3)
  }
  test("reverse") {
    val res = l.reverse
    assert(res == List(2, 1))
  }
  test("foldRight") {
    val res = l.foldRight("") { (a, b) => b + a }
    assert(res == "21")
  }
  test("++") {
    assert(l ++ l == List(1, 2, 1, 2))
  }
  test("map") {
    assert(l.map(_ * 2) == List(2, 4))
  }
  test("flatten") {
    assert(List(List(1, 2), List(3, 4)).flatten == List(1, 2, 3, 4))
  }
  test("flatMap") {
    assert(l.flatMap(x => List(x, x * 2)) == List(1, 2, 2, 4))
  }
  test("toString") {
    assert(l.toString == "List(1,2)")
  }
  test("sum") {
    assert(l.sum == 3)
  }
  test("product") {
    assert(l.product == 2)
  }
