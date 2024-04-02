package tasks.adts

class SubmissionTest {
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

        @Test def testTeacherByName() =
        val newSchool = school.addTeacher("teacher")
        assertEquals(
            Just(Teacher("teacher", Sequence.Nil())),
            newSchool.teacherByName("teacher")
        )

        @Test def testCourseByName() =
        val newSchool = school.addCourse("biology")
        assertEquals(
            Just(Course("biology")),
            newSchool.courseByName("biology")
        )

        @Test def testSetTeacherToCourse() =
        val newSchool = school.setTeacherToCourse(
            Teacher("teacher", Sequence.Nil()),
            Course("biology")
        )
        assertEquals(newSchool.courses.productElement(0), (Course("biology")))

    class Ex3StackTests:
        @Test def testPush() =
        val stack = Ex3Stacks.StackImpl.empty[Int]
        assertEquals(Cons(10, Nil()), stack.push(10))
        assertEquals(
            Cons(20, Cons(10, Nil())),
            stack.push(10).push(20)
        )

        @Test def testPop() =
        val stack = Ex3Stacks.StackImpl.empty[Int]
        assertEquals(Empty(), stack.pop(10))
        assertEquals(Just((10, Nil())), stack.push(10).pop(10))
        assertEquals(Just((20, Cons(10, Nil()))), stack.push(10).push(20).pop(20))

    class Ex5TraversableTest:
        @Test def testLogAllOptionals() =
        val opt = Just(5)
        assertEquals((), logAll(opt))
        val opt2 = Empty()
        assertEquals((), logAll(opt2))

        @Test def testLogAllSequences() =
        val seq = Cons(10, Cons(20, Cons(30, Nil())))
        assertEquals((), logAll(seq))

}
