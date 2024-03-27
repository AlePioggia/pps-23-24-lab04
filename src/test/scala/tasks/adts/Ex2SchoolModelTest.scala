package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.SchoolImpl
import u03.Sequences.Sequence
import tasks.adts.SchoolModel.SchoolImpl.Teacher

class Ex2SchoolModelTest:
  val SchoolModel = SchoolImpl
  val school = SchoolModel.School(Sequence.Nil(), Sequence.Nil())

  @Test def testAddTeacher() =
    val school = SchoolModel.School(Sequence.Nil(), Sequence.Nil())
    val newSchool = school.addTeacher("teacher")
    assertEquals(
      Sequence.Cons(Teacher("teacher", Sequence.Nil()), Sequence.Nil()),
      newSchool.teachers
    )

  @Test def testAddCourse() =
    val newSchool = school.addCourse("biology")
    assertEquals(
      Sequence.Cons(SchoolModel.Course("biology"), Sequence.Nil()),
      newSchool.courses
    )
