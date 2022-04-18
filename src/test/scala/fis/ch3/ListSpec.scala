package fis.ch3
import List._

class ListSpec extends munit.FunSuite:
  test("cons `::` method works") {
    val l = 1 :: Nil
    assert(l.equals(List.Cons(1, Nil)))
  }
