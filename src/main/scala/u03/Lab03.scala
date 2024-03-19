package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.Modules.isStudent
import u02.Modules.*
import u02.Modules.Person.*
import u03.Sequences.Sequence
import u03.Stream.iterate

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

      // Task 2: svolto da solo
      def min: Optional[Int] = ???

    extension [A](l: Sequence[A])
      def map[B](mapper: A => B): Sequence[B] =
        l.flatMap(k => Cons(mapper(k), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        l.flatMap(k =>
          k match
            case k if pred(k) => Cons(k, Nil())
            case _            => Nil()
        )

      // Task 1a - 1b - 1c - 1d: svolto da solo
      def zip[B](second: Sequence[B]): Sequence[(A, B)] =
        (l, second) match
          case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
          case _                            => Nil()

      def take(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
        case _                   => Nil()

      def concat(l2: Sequence[A]): Sequence[A] =
        (l, l2) match
          case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, t1.concat(l2))
          case (Cons(h1, t1), Nil())        => Cons(h1, t1)
          case (Nil(), Cons(h2, t2))        => Cons(h2, t2)
          case _                            => Nil()

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] =
        l match
          case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
          case _          => Nil()

      def foldLeft[B](b: B)(bin: (B, A) => B): B = l match
        case Nil()      => b
        case Cons(h, t) => t.foldLeft(bin(b, h))(bin)

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  import Sequence.*
  // Task 3: svolto da solo
  extension (l: Sequence[Person])
    def takeCourseFromTeacher: Sequence[String] =
      l.flatMap(p =>
        p match
          case Teacher(n, c) => Cons(c, Nil())
          case _             => Nil()
      )

//Task 6 - 7 - 8
enum Stream[A]:
  private case Empty()
  private case Cons(head: () => A, tail: () => Stream[A])

object Stream:

  def empty[A](): Stream[A] = Empty()

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def toList[A](stream: Stream[A]): Sequence[A] = stream match
    case Cons(h, t) => Sequence.Cons(h(), toList(t()))
    case _ => Sequence.Nil()

  def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
    case Cons(head, tail) => cons(f(head()), map(tail())(f))
    case _ => Empty()

  def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
    case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
    case Cons(head, tail) => filter(tail())(pred)
    case _ => Empty()

  def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
    case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
    case _ => Empty()

  def iterate[A](init: => A)(next: A => A): Stream[A] =
    cons(init, iterate(next(init))(next))

  // Task 6
  extension [A](s: Stream[A])
    def takeWhile(p: A => Boolean): Stream[A] = s match
      case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
      case _ => Empty()

  //Task 7
  def fill[A](n: Int)(x: A): Stream[A] = (n > 0) match
    case true => cons(x, fill(n - 1)(x))
    case false => Empty()
    
      

end Stream

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  //println(Sequence.sum(l)) // 30

  import Sequence.*

  //println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
