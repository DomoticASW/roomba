package state

object State:
  opaque type State[S, A] = StateImpl[S, A]
  private case class StateImpl[S, A](run: S => (S, A))

  object State:
    def apply[S, A](run: S => (S, A)): State[S, A] = StateImpl(run)
    def same[S]: State[S, Unit] = State(s => (s, ()))
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
