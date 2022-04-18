package fis.ch3
import List._

class ListSpec extends munit.FunSuite:
  test("cons `::`") {
    val l = 1 :: Nil
    assert(l.equals(List.Cons(1, Nil)))
  }
  test("folLeft") {
    val l = 1 :: 2 :: Nil
    assert(l.foldLeft(0) { (b, a) => b + a } == 3)
  }
