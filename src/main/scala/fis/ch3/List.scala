package fis.ch3

import scala.annotation.{tailrec, targetName}
import Numeric._

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

  override def toString: String =
    s"List(${foldLeft("")((s: String, a: A) =>
        s + s"${if (s.isEmpty) "" else ","}$a"
      )})"

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

  final def map[B](f: A => B): List[B] =
    foldRight[List[B]](Nil)((a, b) => f(a) :: b)

  final def flatMap[B](f: A => List[B]): List[B] = map(f).flatten

  def headOption: Option[A] = this match
    case Cons(a, _) => Some(a)
    case Nil        => None

  def tailOption: Option[List[A]] = this match
    case Cons(_, as) => Some(as)
    case Nil         => None

  def initOption = reverse.tailOption.map(_.reverse)

  def length: Int = foldLeft(0) { (count, _) => count + 1 }

  @targetName("append")
  def ++[B >: A](bs: List[B]): List[B] = foldRight(bs)(_ :: _)

  def filter(p: A => Boolean) = flatMap(a => if (p(a)) List(a) else Nil)

  def zip[B](bs: List[B]): List[(A, B)] =
    foldLeft(Nil: List[(A, B)], bs) {
      case ((l, Cons(b, bs)), a) => ((a, b) :: l, bs)
      case ((l, Nil), _)         => (l, bs)
    }._1.reverse

  def combine[B, C](bs: List[B])(f: (A, B) => C): List[C] =
    zip(bs).map(f.tupled)

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
