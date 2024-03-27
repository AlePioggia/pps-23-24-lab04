package tasks.adts
import u03.Sequences.*
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import u03.Optionals.*
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import u02.AlgebraicDataTypes.Person

/*  Exercise 2:
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion:
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school
 */
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
      def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
