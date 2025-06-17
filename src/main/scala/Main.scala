import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import scala.concurrent.ExecutionContext
import ports.ServerCommunicationProtocol.*
import domain.Roomba.*
import domain.RoombaAgent
import adapters.DomoticASWDeviceHttpInterface
import adapters.ServerCommunicationProtocolHttpAdapter

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

  object isInt:
    def unapply(s: String): Option[Int] = s.toIntOption

  def parseServerAddress: Either[String, Option[ServerAddress]] =
    def stringToServerAddress(s: String): Either[String, ServerAddress] =
      s.split(":").toList match
        case host :: (isInt(port) :: next) => Right(ServerAddress(host, port))
        case _ => Left(s"Invalid server address \"$s\"")

    for
      serverAddressStr <- Right(sys.env.get("SERVER_ADDRESS"))
      serverAddress <- serverAddressStr match
        case Some(value) => stringToServerAddress(value).map(Some(_))
        case None        => Right(None)
    yield (serverAddress)

  def parsePort(default: Int): Either[String, Int] =
    sys.env.get("PORT") match
      case None                                  => Right(default)
      case Some(isInt(p)) if p >= 0 & p <= 65335 => Right(p)
      case Some(isInt(p)) => Left(s"Invalid port $p is out of valid port range")
      case Some(nonInt)   => Left(s"Invalid port $nonInt is not an integer")

  val config = for
    id <- Right(sys.env.get("ID").getOrElse("roomba"))
    name <- Right(sys.env.get("NAME").getOrElse("Roomba"))
    battery <- parseBattery
    batteryRateMs <- parseBatteryRate
    mode <- parseMode
    changeRoomRateMs <- parseChangeRoomRate
    rooms <- parseRooms
    initRoom <- Right(sys.env.get("INIT_ROOM").getOrElse(rooms.head))
    chargingRoom <- Right(sys.env.get("CHARGING_ROOM").getOrElse(rooms.last))
    initialState <- parseInitialState
    serverAddress <- parseServerAddress
    port <- parsePort(default = 8080)
    roomba <- Roomba(
      id,
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
  yield (roomba, serverAddress, port)

  config match
    case Left(err: String) =>
      Console.err.println(err)
      sys.exit(1)
    case Right(roomba, serverAddress, port) =>
      val ec = ExecutionContext.global
      val roombaAgent = RoombaAgent(
        new ServerCommunicationProtocolHttpAdapter(using ec),
        roomba,
        50
      )
      serverAddress.foreach(addr => roombaAgent.registerToServer(addr))
      roombaAgent.start()

      given ActorSystem[Any] = ActorSystem(Behaviors.empty, "system")
      DomoticASWDeviceHttpInterface("0.0.0.0", port, roombaAgent)
