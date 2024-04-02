package tasks.adts

import u03.Sequences.*
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import u03.Optionals.*
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import u04.monads.Monads.Monad
//Task 1
object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class Complex(re: Double, im: Double)
    def complex(re: Double, im: Double): Complex = Complex(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = other match
        case Complex(re, im) => Complex(complex.re + re, complex.im + im)
      def subtract(other: Complex): Complex = other match
        case Complex(re, im) => Complex(complex.re - re, complex.im - im)
      def asString(): String = complex match
        case Complex(re, im) if im > 0 =>
          re.toString().concat(" + ".concat(im.toString().concat("i")))
        case Complex(re, im) if im < 0 =>
          complex.re
            .toString()
            .concat(" - ".concat(Math.abs(im).toString().concat("i")))

//Task 2
object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolImpl extends SchoolModule:
    case class Teacher(name: String, courses: Sequence[Course])
    case class Course(name: String)
    case class School(teachers: Sequence[Teacher], courses: Sequence[Course])
    extension (school: School)
      def addTeacher(name: String): School = school match
        case School(t, c) =>
          School(Cons(Teacher(name, Nil()), t), c)
      def addCourse(name: String): School = school match
        case School(t, c) =>
          School(t, Cons(Course(name), Nil()))
      def teacherByName(name: String): Optional[Teacher] = school match
        case School(t, _) =>
          t match
            case Cons(n, c) if name.equals(n.name) => Just(n)
            case _                                 => Empty()
      def courseByName(name: String): Optional[Course] = school match
        case School(_, c) =>
          c match
            case Cons(n, _) if n.name.equals(name) => Just(n)
            case _                                 => Empty()
      def nameOfTeacher(teacher: Teacher): String = teacher.name
      def nameOfCourse(course: Course): String = course.name
      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        school match
          case School(t, c) => School(t, Cons(course, c))
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        teacher.courses

//Task 3

object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A]
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    opaque type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Sequence.Nil()
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = Sequence.Cons(a, stack)
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) if h == a => Just((h, t))
        case _                    => Empty()

      def asSequence(): Sequence[A] = stack

//Task 4
object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _          => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    seq match
      case Cons(h, t) => summable.sum(h, sumAll(t))
      case _          => summable.zero

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1.concat(a2)
    def zero: String = ""

//Task 5
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

//Task 6
object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = try success(expression)
  catch failure(_)

  extension [A](m: Try[A])
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_)     => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = success(value)

    extension [A](m: Try[A])
      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(value)     => f(value)
        case TryImpl.Failure(exception) => TryImpl.Failure(exception)
