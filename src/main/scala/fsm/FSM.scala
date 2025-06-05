package fsm

import state.State.*

object FSM:

  opaque type FSM[S, D, E] = FSMImpl[S, D, E]
  private case class FSMImpl[S, D, E](s: S, d: D, c: Countdowns)

  private type Countdowns = Map[String, Countdown]
  private case class Countdown(value: Long, resetValue: Long):
    def reset: Countdown = Countdown(resetValue, resetValue)
    def decrement(ms: Long): Countdown = Countdown(value - ms, resetValue)

  extension [S, D, E](fsm: FSM[S, D, E])
    def step(ms: Long, e: Option[E])(using S: States[S, D, E]): FSM[S, D, E] =
      val step = for
        _ <- updateCountdowns(ms)
        prevState <- S.currentState
        newState <- S.onActive(e, ms)
        _ <-
          if newState != prevState then
            for
              _ <- S.onExit()
              _ <- State.modify((_: FSM[S, D, E]).copy(s = newState))
              _ <- S.onEntry()
            yield ()
          else State.same
      yield (newState)
      step.run(fsm)._1

  object FSM:
    def apply[S, D, E](s: S, d: D)(using S: States[S, D, E]): FSM[S, D, E] =
      val fsm1 = FSMImpl[S, D, E](s, d, Map.empty)
      S.onEntry().run(fsm1)._1

    /* UTILS FOR WORKING WITH STATE */

    def modified[S, D, E](f: D => D): State[FSM[S, D, E], Unit] =
      State.modify(fsm => fsm.copy(d = f(fsm.d)))
    def inspect[S, D, E, A](f: D => A): State[FSM[S, D, E], A] =
      State.inspect(fsm => f(fsm.d))
    def same[S, D, E]: State[FSM[S, D, E], Unit] = State.same

    def matchCurrentState[S, D, E, A](
        f: S => State[FSM[S, D, E], A]
    ): State[FSM[S, D, E], A] =
      for
        s <- currentState
        res <- f(s)
      yield (res)

    def currentState[S, D, E]: State[FSM[S, D, E], S] = State.inspect(_.s)

    /* COUNTDOWNS BASICS */

    def countdownReached[S, D, E](name: String): State[FSM[S, D, E], Boolean] =
      State.inspect(_.c.get(name).map(_.value <= 0).getOrElse(false))

    def setCountdown[S, D, E](
        name: String,
        ms: Long
    ): State[FSM[S, D, E], Unit] =
      State.modify(fsm => fsm.copy(c = fsm.c + (name -> Countdown(ms, ms))))

    def resetCountdown[S, D, E](name: String): State[FSM[S, D, E], Unit] =
      State.modify(fsm => fsm.copy(c = fsm.c.updatedWith(name)(_.map(_.reset))))

    /* COUNTDOWNS UTILS */

    def resetCountdownIfReached[S, D, E](
        name: String
    ): State[FSM[S, D, E], Boolean] =
      for
        reached <- countdownReached(name)
        _ <- if reached then resetCountdown(name) else State.same
      yield (reached)
    def setCountdownIfReached[S, D, E](
        name: String,
        ms: Long
    ): State[FSM[S, D, E], Boolean] =
      for
        reached <- countdownReached(name)
        _ <- if reached then setCountdown(name, ms) else State.same
      yield (reached)

    def ifCountdownReached[S, D, E, A](
        name: String
    )(f: State[FSM[S, D, E], A]): State[FSM[S, D, E], Option[A]] =
      for
        reached <- countdownReached(name)
        res <-
          if reached then f.map(a => Some(a))
          else State.pure(None)
      yield (res)

  private def updateCountdowns[S, D, E](ms: Long): State[FSM[S, D, E], Unit] =
    State.modify(fsm => fsm.copy(c = fsm.c.mapValues(_.decrement(ms)).toMap))

  trait States[S, D, E]:
    def currentState: State[FSM[S, D, E], S] = State.inspect(_.s)
    def onEntry(): State[FSM[S, D, E], Unit] = State.same
    def onActive(e: Option[E], timePassed: Long): State[FSM[S, D, E], S] =
      currentState
    def onExit(): State[FSM[S, D, E], Unit] = State.same
