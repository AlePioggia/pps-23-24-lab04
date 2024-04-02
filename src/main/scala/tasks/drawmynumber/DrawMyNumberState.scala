package tasks.drawmynumber

import u04.monads.States.*
import scala.util.Random

enum CurrentGameState {
  case Playing, Won, Lost, Standby
}

val GAME_OVER_VALUE: Int = -1

case class GameStateData(
    secret: Int,
    attempts: Int,
    currentState: CurrentGameState
)

trait DrawMyNumberState:
  type GameState
  def initialGameState(): GameState
  def startNewGame(): State[GameState, Unit]
  def startNewTrialGame(): State[GameState, Unit]
  def guessNumber(n: Int): State[GameState, Unit]
  def getGameState(): State[GameState, CurrentGameState]
  def nop(): State[GameState, Unit]

object DrawMyNumberStateImpl extends DrawMyNumberState:
  opaque type GameState = GameStateData
  def initialGameState(): GameState =
    GameStateData(0, 0, CurrentGameState.Standby)
  def startNewGame(): State[GameState, Unit] =
    State(g =>
      (
        g.copy(
          secret = Random.nextInt(10),
          attempts = 10,
          currentState = CurrentGameState.Playing
        ),
        ()
      )
    )
  def startNewTrialGame(): State[GameState, Unit] =
    State(g =>
      (
        g.copy(
          secret = 1,
          attempts = 5,
          currentState = CurrentGameState.Playing
        ),
        ()
      )
    )
  def guessNumber(n: Int): State[GameState, Unit] = n match
    case n if n < 0 || n > 10 => nop()
    case n =>
      State(g => {
        val newGameState = g.copy(attempts = g.attempts - 1)
        val newState = newGameState match
          case GameStateData(secret, attempts, _)
              if attempts <= GAME_OVER_VALUE =>
            CurrentGameState.Lost
          case GameStateData(secret, attempts, _)
              if secret == n && attempts >= GAME_OVER_VALUE =>
            CurrentGameState.Won
          case _ => CurrentGameState.Playing
        (newGameState.copy(currentState = newState), ())
      })
  def getGameState(): State[GameState, CurrentGameState] =
    State(g => (g, g.currentState))
  def nop(): State[GameState, Unit] = State(g => (g, ()))

@main def tryDrawMyNumberState =
  val drawMyNumberState: DrawMyNumberState = DrawMyNumberStateImpl
  import drawMyNumberState.* // or directly, import DrawMyNumberStateImpl.*

  val session: State[GameState, CurrentGameState] =
    for
      _ <- startNewTrialGame()
      _ <- guessNumber(5)
      _ <- guessNumber(4)
      _ <- guessNumber(3)
      _ <- guessNumber(1)
      _ <- guessNumber(0)
      _ <- guessNumber(0)
      v <- getGameState()
    yield v

  println:
    session.run(initialGameState()) // (GameStateData(1, 5, Won), Won
