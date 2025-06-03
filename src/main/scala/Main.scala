import domain.Roomba
import domain.Roomba.*

// object MainSimulation extends App:
//   import domain.Simulation
//   import domain.Simulation.SimulationListener
//   import domain.Simulation.Input
//   val initialState = Roomba(
//     "R",
//     50,
//     Mode.Silent,
//     "Kitchen",
//     "Bedroom",
//     Set("Kitchen", "Bedroom", "Livingroom", "Bathroom")
//   ).right.get

//   val simulation = Simulation(
//     initialState,
//     0,
//     0,
//     500,
//     Set(new SimulationListener {

//       override def onSimulationStep(r: Roomba): Unit = println(r)

//     })
//   )
//   simulation.start()

//   Thread.sleep(5000)
//   simulation.enqueueInputs(Input.Start)
//   Thread.sleep(10000)
//   simulation.enqueueInputs(Input.Stop)

import fsm.RoombaFSM.*
import fsm.FSM.*
object MainFSM extends App:

  val roomba = Roomba(
    "R",
    50,
    Mode.Silent,
    "Kitchen",
    "Bedroom",
    Set("Kitchen", "Bedroom", "Livingroom", "Bathroom")
  ).right.get
  val model = Model(roomba, 500, 0, 2000, 0)

  var fsm = FSM(State.Cleaning, model)
  while true do
    val period = 50
    Thread.sleep(period)
    fsm = fsm.step(period, None)
    println(fsm)
