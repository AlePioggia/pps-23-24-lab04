package tasks.drawmynumber

import u04.monads.States.*
import tasks.drawmynumber.WindowStateImpl.Window
import u03.extensionmethods.Streams.*
import u04.monads.Monads.*, u04.monads.Monads.Monad.*, u04.monads.States.*,
  State.*, WindowStateImpl.*
import tasks.drawmynumber.DrawMyNumberStateImpl.startNewGame
import tasks.drawmynumber.DrawMyNumberStateImpl.getGameState
import tasks.drawmynumber.DrawMyNumberStateImpl.initialGameState
import tasks.drawmynumber.DrawMyNumberStateImpl.guessNumber
import tasks.drawmynumber.DrawMyNumberStateImpl.startNewTrialGame
import tasks.drawmynumber.DrawMyNumberStateImpl.nop

@main def runMVC =
  def mv[SM, SV, AM, AV](
      m1: State[SM, AM],
      f: AM => State[SV, AV]
  ): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(300, 300)
    _ <- addButton(text = "draw", name = "DrawButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- addTextField()
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(
      seq(startNewTrialGame(), getGameState()),
      i => windowCreation(i.toString())
    )
    _ <- seqN(events.map(_ match
      case name if name.forall(_.isDigit) && name.toIntOption.exists(_ >= 0) =>
        mv(
          seq(guessNumber(name.toInt), getGameState()),
          i => toLabel(i.toString, "Label1")
        )
    ))
  yield ()

  controller.run((initialGameState(), initialWindow))
