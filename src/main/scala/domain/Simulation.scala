// package domain

// import scala.util.Random
// import Roomba.*
// import Roomba.State.*

// object Simulation:
//   trait SimulationListener:
//     def onSimulationStep(r: Roomba): Unit

//   enum Input:
//     case ChangeMode(m: Mode)
//     case Start
//     case Stop

// import Simulation.*

// class Simulation(
//     private var r: Roomba,
//     private val timeToChangeRoomSeconds: Int,
//     private val batteryUnitDurationSeconds: Int,
//     private val periodMillis: Int,
//     private var listeners: Set[SimulationListener]
// ) extends Thread:
//   private var inputs = Seq[Input]()
//   private var _shouldStop = false
//   private def shouldStop: Boolean =
//     synchronized { _shouldStop }
//   def setShoulStop(): Unit =
//     synchronized { _shouldStop = true }

//   def enqueueInputs(i: Input*) =
//     synchronized { inputs = inputs concat i }

//   def addListener(l: SimulationListener) =
//     synchronized { listeners = listeners + l }
//   def removeListener(l: SimulationListener) =
//     synchronized { listeners = listeners - l }

//   private var changeRoomCountdown = timeToChangeRoomSeconds * 1000
//   private var batteryCountdown = batteryUnitDurationSeconds * 1000
//   private def resetChangeRoomCountdown() =
//     changeRoomCountdown = timeToChangeRoomSeconds * 1000
//   private def resetBatteryCountdown() =
//     batteryCountdown = batteryUnitDurationSeconds * 1000

//   override def run(): Unit =
//     while !shouldStop do
//       // Dequeue all inputs
//       var inputs = Seq[Input]()
//       synchronized:
//         inputs = this.inputs
//         this.inputs = Seq()

//       for i <- inputs do
//         i match
//           case Input.ChangeMode(m) =>
//             r = r.update(mode = m)
//           case Input.Start =>
//             r = r.update(state = Cleaning)
//           case Input.Stop =>
//             r = r.update(state = GoingCharging)

//       r.state match
//         case Charging =>
//           if r.battery < 100 then
//             if batteryCountdown <= 0 then
//               r = r.update(battery = r.battery + 1)
//               resetBatteryCountdown()
//             else batteryCountdown = batteryCountdown - periodMillis
//           else resetBatteryCountdown()
//         case Cleaning =>
//           if batteryCountdown <= 0 then
//             val newBattery = r.battery - 1
//             r = r.update(
//               battery = newBattery,
//               state = if newBattery < 10 then GoingCharging else Cleaning
//             )
//             resetBatteryCountdown()
//           else batteryCountdown = batteryCountdown - periodMillis
//           if changeRoomCountdown <= 0 then
//             r = r.update(currentRoom = Random().shuffle(r.rooms).head)
//             resetChangeRoomCountdown()
//           else changeRoomCountdown = changeRoomCountdown - periodMillis
//         case GoingCharging =>
//           if r.chargingStationRoom == r.currentRoom then
//             r = r.update(state = Charging)
//           else if changeRoomCountdown <= 0 then
//             r = r.update(currentRoom = Random().shuffle(r.rooms).head)
//             resetChangeRoomCountdown()
//           else changeRoomCountdown = changeRoomCountdown - periodMillis

//       listeners.foreach(_.onSimulationStep(r))
//       Thread.sleep(periodMillis)
