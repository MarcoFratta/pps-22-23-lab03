package lab

import u02.Modules.Person
import u02.Modules.Person.{Student, Teacher}
import u03.Lists.List
import u03.Lists.List.Cons
import u03.Streams.Stream
import u03.Streams.Stream.{Cons, Empty}

import scala.annotation.tailrec

object Task1 extends App:


  import u03.Lists.*
  import List.*
  import u02.Optionals.*
  import Option.*


  // TASK 1a, SVOLTO DA SOLO
  def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(_, t) if n == 0 => t
    case Nil() => Nil()
    case Cons(h, t) => Cons(h, drop(t, n - 1))

  // TASK 1b, SVOLTO DA SOLO
  def append[A](left: List[A], right: List[A]): List[A] = left match
    case Nil() => right
    case Cons(h, t) => Cons(h, append(t, right))

  // TASK 1c, SVOLTO DA SOLO
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
    case Nil() => Nil()
    case Cons(h, t) => append(f(h), flatMap(t)(f))

  // TASK 1d, SVOLTO DA SOLO
  def map2[A, B](l: List[A])(f: A => B): List[B] =
    flatMap(l)(x => Cons(f(x), Nil()))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => f(a) match
      case true => Cons(a, Nil())
      case _ => Nil())

  // TASK 2, SVOLTO DA SOLO
  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None()
    case Cons(h, t) => max(t) match
      case Some(x) if x > h => Some(x)
      case _ => Some(h)


  // TASK 3, SVOLTO DA SOLO
  def personCourses(l: List[Person]): List[String] =
    flatMap(l)(x => x match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil())

  // TASK 4, SVOLTO DA SOLO
  @tailrec
  def foldLeft[A, B](l: List[A])(defVal: B)(f: (B, A) => B): B = l match
    case Nil()=> defVal
    case Cons(h, t) => foldLeft(t)(f(defVal, h))(f)

  def foldRight[A, B](l: List[A])(defVal: B)(f: (A, B) => B): B =
    foldLeft(reverse(l))(defVal)((a, x) => f(x, a))

  private def reverse[A](l: List[A]): List[A] =
    foldLeft(l)(Nil())((head, tail) => Cons(tail, head))

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

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


    // TASK 5, SVOLTO DA SOLO
    @tailrec
    def drop[A](s: Stream[A])(n: Int): Stream[A] = (s, n) match
      case (s, 0) => s
      case (s, n) if n < 0 => s
      case (Empty(), _) => Empty()
      case (Cons(_, t), n) => drop(t())(n - 1)

    // TASK 6, SVOLTO DA SOLO

    def constant[A](k: A): Stream[A] = cons(k, constant(k))

    def fib(): Stream[Int] = fibonacciSeq(0, 1)

    // TASK 7, SVOLTO DA SOLO
    private def fibonacciSeq(a: Int, b: Int): Stream[Int] =
      Cons(() => a, () => fibonacciSeq(b, a + b))

  end Stream

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

