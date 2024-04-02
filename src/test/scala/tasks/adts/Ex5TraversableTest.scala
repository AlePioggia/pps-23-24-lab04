package tasks.adts

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import tasks.typeclasses.Ex5Traversable.logAll

class Ex5TraversableTest:
  @Test def testLogAllOptionals() =
    val opt = Just(5)
    assertEquals((), logAll(opt))
    val opt2 = Empty()
    assertEquals((), logAll(opt2))

  @Test def testLogAllSequences() =
    val seq = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals((), logAll(seq))
