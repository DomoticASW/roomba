import fsm.RoombaFSM
import fsm.FSM.*
import domain.Roomba.*

object MyRoombaFSM
    extends fsm.RoombaFSM(
      batteryRateMs = 1000,
      changeRoomRateMs = 4000
    )

object MainFSM extends App:
  import MyRoombaFSM.{*, given}

  val roomba = Roomba(
    "R",
    50,
    Mode.Silent,
    "Kitchen",
    "Bedroom",
    Set("Kitchen", "Bedroom", "Livingroom", "Bathroom")
  ).right.get

  var fsm = FSM(State.Cleaning, roomba)
  while true do
    val period = 50
    Thread.sleep(period)
    fsm = fsm.step(period, None)
    println(fsm)
