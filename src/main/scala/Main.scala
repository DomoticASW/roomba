import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import scala.concurrent.ExecutionContext
import ports.ServerCommunicationProtocol.*
import domain.Roomba.*
import domain.RoombaAgent
import adapters.DomoticASWDeviceHttpInterface
import adapters.ServerCommunicationProtocolHttpAdapter

object MainFSM extends App:
  def parse(envVar: String)(default: String): Right[Nothing, String] =
    Right(sys.env.getOrElse(envVar, default))

  def parseServerDiscoveryPort(): Either[String, Int] =
    val envVar = "SERVER_DISCOVERY_PORT"
    for
      str <- sys.env.get(envVar).toRight(left = s"Missing a value for $envVar")
      port <- str.toIntOption match
        case None => Left(s"Invalid port $str is not an integer")
        case Some(p) if p >= 0 & p <= 65335 => Right(p)
        case Some(p) => Left(s"Invalid port $p is out of valid port range")
    yield (port)

  def parseBattery(default: Int): Either[String, Int] =
    for
      batteryStr <- Right(sys.env.get("BATTERY"))
      battery <- batteryStr match
        case None => Right(default)
        case Some(value) =>
          value.toIntOption.toRight("Battery should be integer")
    yield (battery)

  def parseBatteryRate(default: Long): Either[String, Long] =
    for
      batteryRateMsStr <- Right(sys.env.get("BATTERY_RATE_MS"))
      batteryRateMs <- batteryRateMsStr match
        case None => Right(default)
        case Some(value) =>
          value.toLongOption.toRight("Battery rate should be integer")
    yield (batteryRateMs)

  def parseChangeRoomRate(default: Long): Either[String, Long] =
    for
      changeRoomRateMsStr <- Right(sys.env.get("CHANGE_ROOM_RATE_MS"))
      changeRoomRateMs <- changeRoomRateMsStr match
        case None => Right(default)
        case Some(value) =>
          value.toLongOption.toRight("Change room rate should be integer")
    yield (changeRoomRateMs)

  def parseMode(default: Mode): Either[String, Mode] =
    for
      modeStr <- Right(sys.env.get("MODE"))
      mode <- modeStr match
        case Some("Silent")       => Right(Mode.Silent)
        case Some("DeepCleaning") => Right(Mode.DeepCleaning)
        case Some("Performance")  => Right(Mode.Performance)
        case None                 => Right(default)
        case Some(other) => Left(s"$other is not a valid value for MODE")
    yield (mode)

  def parseRooms(default: Set[String]): Either[String, Set[String]] =
    for
      roomsStr <-
        Right(sys.env.get("ROOMS").map(_.split(",").map(_.trim()).toSet))
      rooms <- roomsStr match
        case None                          => Right(default)
        case Some(value) if value.size > 1 => Right(value)
        case _ => Left("At least one room should be given")
    yield (rooms)

  def parseInitialState(default: State): Either[String, State] =
    for
      stateStr <- Right(sys.env.get("STATE"))
      state <- stateStr match
        case Some("Charging")      => Right(State.Charging)
        case Some("GoingCharging") => Right(State.GoingCharging)
        case Some("Cleaning")      => Right(State.Cleaning)
        case None                  => Right(default)
        case Some(other) => Left(s"$other is not a valid value for STATE")
    yield (state)

  object isInt:
    def unapply(s: String): Option[Int] = s.toIntOption

  def parseServerAddress(
      default: Option[ServerAddress]
  ): Either[String, Option[ServerAddress]] =
    def stringToServerAddress(s: String): Either[String, ServerAddress] =
      s.split(":").toList match
        case host :: (isInt(port) :: next) => Right(ServerAddress(host, port))
        case _ => Left(s"Invalid server address \"$s\"")

    for
      serverAddressStr <- Right(sys.env.get("SERVER_ADDRESS"))
      serverAddress <- serverAddressStr match
        case Some(value) => stringToServerAddress(value).map(Some(_))
        case None        => Right(default)
    yield (serverAddress)

  def parsePort(default: Int): Either[String, Int] =
    sys.env.get("PORT") match
      case None                                  => Right(default)
      case Some(isInt(p)) if p >= 0 & p <= 65335 => Right(p)
      case Some(isInt(p)) => Left(s"Invalid port $p is out of valid port range")
      case Some(nonInt)   => Left(s"Invalid port $nonInt is not an integer")

  val config = for
    id <- parse("ID")(default = "roomba")
    name <- parse("NAME")(default = "Roomba")
    battery <- parseBattery(default = 50)
    batteryRateMs <- parseBatteryRate(default = 1000)
    mode <- parseMode(default = Mode.Performance)
    changeRoomRateMs <- parseChangeRoomRate(default = 4000)
    rooms <- parseRooms(default =
      Set("Kitchen", "Bedroom", "Livingroom", "Bathroom")
    )
    initRoom <- parse("INIT_ROOM")(default = rooms.head)
    chargingRoom <- parse("CHARGING_ROOM")(default = rooms.last)
    serverDiscoveryPort <- parseServerDiscoveryPort()
    initialState <- parseInitialState(default = State.Cleaning)
    serverAddress <- parseServerAddress(default = None)
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
  yield (roomba, serverAddress, port, serverDiscoveryPort)

  config match
    case Left(err: String) =>
      Console.err.println(err)
      sys.exit(1)
    case Right(roomba, serverAddress, port, serverDiscoveryPort) =>
      val ec = ExecutionContext.global
      val roombaAgent = RoombaAgent(
        new ServerCommunicationProtocolHttpAdapter(
          serverPortToWhichAnnounce = serverDiscoveryPort,
          clientPortToAnnounce = port
        )(using ec),
        roomba,
        periodMs = 50,
        announceEveryMs = 1000
      )
      serverAddress.foreach(addr => roombaAgent.registerToServer(addr))
      roombaAgent.start()

      given ActorSystem[Any] = ActorSystem(Behaviors.empty, "system")
      DomoticASWDeviceHttpInterface("0.0.0.0", port, roombaAgent)
