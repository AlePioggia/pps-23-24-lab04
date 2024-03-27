package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.SchoolImpl
import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import tasks.adts.SchoolModel.SchoolImpl.Teacher
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import Ex3Stacks.StackImpl
import Ex3Stacks.*
import Ex3Stacks.StackImpl.push

class Ex3StackTests:
  @Test def testPush(): Unit = {
    val stack = Ex3Stacks.StackImpl.empty[Int]
    assertEquals(Cons(10, Nil()), stack.push(10))
    assertEquals(
      Cons(20, Cons(10, Nil())),
      stack.push(10).push(20)
    )
  }
