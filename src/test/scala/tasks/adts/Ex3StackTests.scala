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
import Ex3Stacks.StackImpl.pop

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
