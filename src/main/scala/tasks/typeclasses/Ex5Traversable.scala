package tasks.typeclasses
import u03.Sequences.*
import Sequence.*
import u03.Optionals.Optional
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty

/*  Exercise 5:
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others...
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: " + a)

  trait Traversable[T[_]]:
    def computeOne[A](consumer: A => Unit)(elem: A): Unit
    def computeAll[A](consumer: A => Unit)(seq: T[A]): Unit

  given Traversable[Optional] with
    def computeOne[A](consumer: A => Unit)(elem: A): Unit = consumer(elem)
    def computeAll[A](consumer: A => Unit)(seq: Optional[A]): Unit = seq match
      case Just(a) => consumer(a)
      case Empty() => ()

  given Traversable[Sequence] with
    def computeOne[A](consumer: A => Unit)(elem: A): Unit = consumer(elem)
    def computeAll[A](consumer: A => Unit)(seq: Sequence[A]): Unit = seq match
      case Cons(h, t) => computeOne(consumer)(h); computeAll(consumer)(t)
      case _          => ()

  def logAll[A, T[_]](seq: T[A])(using t: Traversable[T]): Unit =
    t.computeAll(log[A])(seq)

  def printlnAll[A, T[_]](seq: T[A])(using t: Traversable[T]): Unit =
    t.computeAll(println)(seq)

  @main def tryTraversable =
    val opt = Just(5)
    logAll(opt)
    printlnAll(opt)

    val seq = Cons(10, Cons(20, Cons(30, Nil())))
    logAll(seq)
    printlnAll(seq)
