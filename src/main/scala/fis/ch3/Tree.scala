package fis.ch3

import scala.annotation.tailrec
import List._

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def fold[B](map: A => B)(reduce: (B, B) => B): B =
    type TU = Tree[A] | Unit
    @tailrec
    def inner(toVisit: List[TU], acc: List[B]): List[B] =
      toVisit match
        case Nil => acc
        case Cons(tu, tus) =>
          tu match
            case Leaf(a: A) =>
              inner(tus, map(a) :: acc)
            case Branch(l, r) =>
              inner(l :: r :: () :: tus, acc)
            case () =>
              inner(tus, acc.take(2).reduce(reduce) :: acc.drop(2))
    inner(List(this), Nil).head

  def size = fold(_ => 1)(_ + _)
