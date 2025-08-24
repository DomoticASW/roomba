package fsm

import state.State.*

object FSM:

  opaque type FSM[S, D, E] = FSMImpl[S, D, E]
  private case class FSMImpl[S, D, E](s: S, d: D, c: Countdowns)

  private type Countdowns = Map[String, Countdown]
  private case class Countdown(value: Long, resetValue: Long):
    def reset: Countdown = Countdown(resetValue, resetValue)
    def decrement(ms: Long): Countdown =
      Countdown(math.max(value - ms, 0), resetValue)

  extension [S, D, E](fsm: FSM[S, D, E])
    def step(ms: Long, e: Option[E])(using S: FSMState[S, D, E]): FSM[S, D, E] =
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

    def data: D = fsm.d

    def state: S = fsm.s

  object FSM:
    def apply[S, D, E](s: S, d: D)(using S: FSMState[S, D, E]): FSM[S, D, E] =
      val fsm1 = FSMImpl[S, D, E](s, d, Map.empty)
      S.onEntry().run(fsm1)._1

  private def updateCountdowns[S, D, E](ms: Long): State[FSM[S, D, E], Unit] =
    State.modify(fsm =>
      fsm.copy(c = fsm.c.view.mapValues(_.decrement(ms)).toMap)
    )

  trait FSMState[S, D, E]:
    type FSMState[A] = State[FSM[S, D, E], A]

    def currentState: FSMState[S] = State.inspect(_.s)

    def onEntry(): FSMState[Unit] = State.same

    def onActive(e: Option[E], timePassed: Long): FSMState[S] = currentState

    def onExit(): FSMState[Unit] = State.same

    /* UTILS FOR WORKING WITH STATE */

    def modified(f: D => D): FSMState[Unit] =
      State.modify(fsm => fsm.copy(d = f(fsm.d)))
    def inspect[A](f: D => A): FSMState[A] =
      State.inspect(fsm => f(fsm.d))
    def same: FSMState[Unit] = State.same

    def matchCurrentState[A](f: S => FSMState[A]): FSMState[A] =
      for
        s <- currentState
        res <- f(s)
      yield (res)

    /* COUNTDOWNS BASICS */

    def countdownReached(name: String): FSMState[Boolean] =
      State.inspect(_.c.get(name).map(_.value <= 0).getOrElse(false))

    def setCountdown(name: String, ms: Long): FSMState[Unit] =
      State.modify(fsm => fsm.copy(c = fsm.c + (name -> Countdown(ms, ms))))

    def resetCountdown(name: String): FSMState[Unit] =
      State.modify(fsm => fsm.copy(c = fsm.c.updatedWith(name)(_.map(_.reset))))

    /* COUNTDOWNS UTILS */

    def resetCountdownIfReached(name: String): FSMState[Boolean] =
      for
        reached <- countdownReached(name)
        _ <- if reached then resetCountdown(name) else State.same
      yield (reached)

    def setCountdownIfReached(name: String, ms: Long): FSMState[Boolean] =
      for
        reached <- countdownReached(name)
        _ <- if reached then setCountdown(name, ms) else State.same
      yield (reached)

    def ifCountdownReached[A](
        name: String
    )(f: FSMState[A]): FSMState[Option[A]] =
      for
        reached <- countdownReached(name)
        res <-
          if reached then f.map(a => Some(a))
          else State.pure(None)
      yield (res)
