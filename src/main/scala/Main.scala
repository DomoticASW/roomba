import domain.Roomba
import domain.Roomba.*
import domain.Simulation
import domain.Simulation.SimulationListener
import domain.Simulation.Input

@main def hello(): Unit =
  val initialState = Roomba(
    "R",
    50,
    State.Charging,
    Mode.Silent,
    "Kitchen",
    "Bedroom",
    Set("Kitchen", "Bedroom", "Livingroom", "Bathroom")
  ).right.get

  val simulation = Simulation(
    initialState,
    0,
    0,
    500,
    Set(new SimulationListener {

      override def onSimulationStep(r: Roomba): Unit = println(r)

    })
  )
  simulation.start()

  Thread.sleep(5000)
  simulation.enqueueInputs(Input.Start)
  Thread.sleep(10000)
  simulation.enqueueInputs(Input.Stop)
