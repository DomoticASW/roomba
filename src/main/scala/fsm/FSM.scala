package fsm

object State:
  opaque type State[S, A] = StateImpl[S, A]
  private case class StateImpl[S, A](run: S => (S, A))

  def apply[S, A](run: S => (S, A)): State[S, A] = StateImpl(run)

  def same[S]: State[S, Unit] = State(s => (s, ()))
  def sameWith[S, A](a: A): State[S, A] = State(s => (s, a))
  def inspect[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
  def modify[S, A](f: S => S): State[S, Unit] = State(s => (f(s), ()))

  extension [S, A](s: State[S, A])
    def run(init: S): (S, A) = s.run(init)
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(a =>
        val (newState, res) = s.run(a)
        f(res).run(newState)
      )
    def map[B](f: A => B): State[S, B] =
      s.flatMap(a => State(init => (init, f(a))))

object FSM:
  import State.*

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

  trait States[S, D, E]:
    def onEntry(s: S): State[D, Unit]
    def onActive(s: S, e: Option[E], timePassed: Long): State[D, S]
    def onExit(s: S): State[D, Unit]
