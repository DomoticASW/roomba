import fsm.RoombaFSM
import fsm.FSM.*
import domain.Roomba.*

def parseRooms(s: String): Set[String] =
  s.split(",").map(_.trim()).toSet
object MainFSM extends App:

  val roomba = for
    name <- Right(sys.env.get("NAME").getOrElse("Roomba"))
    batteryStr <- Right(sys.env.get("BATTERY"))
    battery <- batteryStr match
      case None        => Right(50)
      case Some(value) => value.toIntOption.toRight("Battery should be integer")
    batteryRateMsStr <- Right(sys.env.get("BATTERY_RATE_MS"))
    batteryRateMs <- batteryRateMsStr match
      case None => Right(1000)
      case Some(value) =>
        value.toIntOption.toRight("Battery rate should be integer")
    modeStr <- Right(sys.env.get("MODE"))
    mode <- modeStr match
      case Some("Silent")             => Right(Mode.Silent)
      case Some("DeepCleaning")       => Right(Mode.DeepCleaning)
      case Some("Performance") | None => Right(Mode.Performance)
      case _ => Left(s"$modeStr is not a valid value for MODE")
    changeRoomRateMsStr <- Right(sys.env.get("CHANGE_ROOM_RATE_MS"))
    changeRoomRateMs <- batteryRateMsStr match
      case None => Right(4000)
      case Some(value) =>
        value.toIntOption.toRight("Change room rate should be integer")
    roomsStr <- Right(sys.env.get("ROOMS").map(parseRooms(_)))
    rooms <- roomsStr match
      case None => Right(Set("Kitchen", "Bedroom", "Livingroom", "Bathroom"))
      case Some(value) if value.size > 1 => Right(value)
      case _ => Left("At least one room should be given")
    initRoom <- Right(sys.env.get("INIT_ROOM").getOrElse(rooms.head))
    chargingRoom <- Right(sys.env.get("CHARGING_ROOM").getOrElse(rooms.last))
    roomba <- Roomba(
      name,
      battery,
      mode,
      initRoom,
      chargingRoom,
      rooms,
      batteryRateMs,
      changeRoomRateMs
    )
  yield (roomba)

  roomba match
    case Left(err: String)           => Console.err.println(err)
    case Left(BadConfiguration(err)) => Console.err.println(err)
    case Right(roomba) =>
      import RoombaFSM.{given}

      var fsm = FSM(State.Cleaning, roomba)
      while true do
        val period = 50
        Thread.sleep(period)
        fsm = fsm.step(period, None)
        println(fsm)
