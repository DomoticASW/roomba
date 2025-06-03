package fsm

object FSM:

  opaque type FSM[S, D, E] = FSMImpl[S, D, E]
  private case class FSMImpl[S, D, E](s: S, d: D, t: Long)(using
      States[S, D, E]
  )

  extension [S, D, E](fsm: FSM[S, D, E])
    def state: S = fsm.s
    def data: D = fsm.d
    def time: Long = fsm.t
    def step(ms: Long, env: E)(using states: States[S, D, E]): FSM[S, D, E] =
      val t = time + ms
      val d = states.onActive(state, data, t, ms, env)
      states.nextState(state, d, t, env) match
        case None => fsm.copy(state, d, t)
        case Some(s1, d1) =>
          val d2 = states.onExit(state, d1, t, env)
          fsm.copy(s1, d2, t)

  object FSM:
    def apply[S, D, E](s: S, d: D, e: E)(using
        states: States[S, D, E]
    ): FSM[S, D, E] =
      val t = 0
      FSMImpl(s, states.onEntry(s, d, t, e), t)

  trait States[S, D, E]:
    def onEntry(s: S, d: D, t: Long, e: E): D
    def onActive(s: S, d: D, t: Long, lastActiveT: Long, e: E): D
    def onExit(s: S, d: D, t: Long, e: E): D
    def nextState(s: S, d: D, t: Long, e: E): Option[(S, D)]
