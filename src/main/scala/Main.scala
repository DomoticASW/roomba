import domain.Roomba.*
import domain.RoombaAgent
import adapters.ServerHttpAdapter
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import adapters.DomoticASWDeviceHttpInterface

object MainFSM extends App:
  def parseBattery: Either[String, Int] =
    for
      batteryStr <- Right(sys.env.get("BATTERY"))
      battery <- batteryStr match
        case None => Right(50)
        case Some(value) =>
          value.toIntOption.toRight("Battery should be integer")
    yield (battery)

  def parseBatteryRate: Either[String, Long] =
    for
      batteryRateMsStr <- Right(sys.env.get("BATTERY_RATE_MS"))
      batteryRateMs <- batteryRateMsStr match
        case None => Right(1000)
        case Some(value) =>
          value.toIntOption.toRight("Battery rate should be integer")
    yield (batteryRateMs)

  def parseChangeRoomRate: Either[String, Long] =
    for
      changeRoomRateMsStr <- Right(sys.env.get("CHANGE_ROOM_RATE_MS"))
      changeRoomRateMs <- changeRoomRateMsStr match
        case None => Right(4000)
        case Some(value) =>
          value.toIntOption.toRight("Change room rate should be integer")
    yield (changeRoomRateMs)

  def parseMode: Either[String, Mode] =
    for
      modeStr <- Right(sys.env.get("MODE"))
      mode <- modeStr match
        case Some("Silent")             => Right(Mode.Silent)
        case Some("DeepCleaning")       => Right(Mode.DeepCleaning)
        case Some("Performance") | None => Right(Mode.Performance)
        case Some(other) => Left(s"$other is not a valid value for MODE")
    yield (mode)

  def parseRooms: Either[String, Set[String]] =
    for
      roomsStr <-
        Right(sys.env.get("ROOMS").map(_.split(",").map(_.trim()).toSet))
      rooms <- roomsStr match
        case None => Right(Set("Kitchen", "Bedroom", "Livingroom", "Bathroom"))
        case Some(value) if value.size > 1 => Right(value)
        case _ => Left("At least one room should be given")
    yield (rooms)

  def parseInitialState: Either[String, State] =
    for
      stateStr <- Right(sys.env.get("STATE"))
      state <- stateStr match
        case Some("Charging")        => Right(State.Charging)
        case Some("GoingCharging")   => Right(State.GoingCharging)
        case Some("Cleaning") | None => Right(State.Cleaning)
        case Some(other) => Left(s"$other is not a valid value for STATE")
    yield (state)

  val roomba = for
    name <- Right(sys.env.get("NAME").getOrElse("Roomba"))
    battery <- parseBattery
    batteryRateMs <- parseBatteryRate
    mode <- parseMode
    changeRoomRateMs <- parseChangeRoomRate
    rooms <- parseRooms
    initRoom <- Right(sys.env.get("INIT_ROOM").getOrElse(rooms.head))
    chargingRoom <- Right(sys.env.get("CHARGING_ROOM").getOrElse(rooms.last))
    initialState <- parseInitialState
    roomba <- Roomba(
      initialState,
      name,
      battery,
      mode,
      initRoom,
      chargingRoom,
      rooms,
      batteryRateMs,
      changeRoomRateMs
    ).left.map(_.message)
  yield (roomba)

  roomba match
    case Left(err: String) =>
      Console.err.println(err)
      sys.exit(1)
    case Right(roomba) =>
      val roombaAgent = RoombaAgent(roomba, 50)
      roombaAgent.start()

      given ActorSystem[Any] = ActorSystem(Behaviors.empty, "system")
      DomoticASWDeviceHttpInterface("localhost", 8080, roombaAgent)
