package fis.ch3

import scala.annotation.{tailrec, targetName}
import Numeric._

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

  final def reverse: List[A] = foldLeft[List[A]](Nil)((b, a) => a :: b)

  @targetName("cons")
  final def ::[B >: A](b: B): List[B] = Cons(b, this)

  @tailrec
  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match
      case Nil         => b
      case Cons(a, as) => as.foldLeft(f(b, a))(f)

  final def foldRight[B](b: B)(f: (A, B) => B): B =
    reverse.foldLeft(b)((b, a) => f(a, b))

  @targetName("append")
  final def ++[B >: A](bs: List[B]): List[B] =
    reverse.foldLeft(bs)((b, a) => a :: b)

  final def map[B](f: A => B): List[B] =
    foldRight[List[B]](Nil)((a, b) => f(a) :: b)

  final def flatMap[B](f: A => List[B]): List[B] = map(f).flatten

  override def toString: String =
    s"List(${foldLeft("")((s: String, a: A) =>
        s + s"${if (s.isEmpty) "" else ","}$a"
      )})"
end List

object List:
  extension [A](l: List[List[A]])
    def flatten: List[A] = l.foldRight[List[A]](Nil)((a, b) => a ++ b)

  extension [A](l: List[A])
    def sum(using o: Numeric[A]): A = l.foldLeft(o.zero)((b, a) => o.plus(b, a))
    def product(using o: Numeric[A]): A =
      l.foldLeft(o.one)((b, a) => o.times(b, a))

  def apply[A](as: A*): List[A] = as.foldRight[List[A]](Nil)((a, b) => a :: b)
end List
