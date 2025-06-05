package fsm

import state.State.*

object FSM:

  opaque type FSM[S, D, E] = FSMImpl[S, D, E]
  private case class FSMImpl[S, D, E](s: S, d: D)(using States[S, D, E])

  extension [S, D, E](fsm: FSM[S, D, E])
    def step(ms: Long, e: Option[E])(using States[S, D, E]): FSM[S, D, E] =
      val states = summon[States[S, D, E]]
      val step = for
        newState <- states.onActive(fsm.s, e, ms)
        _ <-
          if newState != fsm.s then
            for
              _ <- states.onExit(fsm.s)
              _ <- states.onEntry(newState)
            yield ()
          else State.same
      yield (newState)
      val (newD, newState) = step.run(fsm.d)
      fsm.copy(s = newState, d = newD)

  object FSM:
    def apply[S, D, E](s: S, d: D)(using States[S, D, E]): FSM[S, D, E] =
      val (d1, _) = summon[States[S, D, E]].onEntry(s).run(d)
      FSMImpl(s, d1)

    def countdownReached[D](name: String): State[D, Boolean] = ???
    def setCountdown[D](name: String, ms: Long): State[D, Unit] = ???
    def resetCountdown[D](name: String): State[D, Unit] = ???
    def resetCountdownIfReached[D](name: String): State[D, Boolean] =
      for
        reached <- countdownReached(name)
        _ <- if reached then resetCountdown(name) else State.same
      yield (reached)
    def setCountdownIfReached[D](name: String, ms: Long): State[D, Boolean] =
      for
        reached <- countdownReached(name)
        _ <- if reached then setCountdown(name, ms) else State.same
      yield (reached)

  trait States[S, D, E]:
    def onEntry(s: S): State[D, Unit]
    def onActive(s: S, e: Option[E], timePassed: Long): State[D, S]
    def onExit(s: S): State[D, Unit]
