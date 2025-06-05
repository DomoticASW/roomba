package fsm
import scala.util.Random
import domain.Roomba.*
import domain.Roomba.State.*

object RoombaFSM:

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop

  import FSM.*

  val batteryRate = 1000
  val changeRoomRate = 4000

  given FSMState[State, Roomba, Event] with
    override def onEntry(): FSMState[Unit] =
      matchCurrentState:
        _ match
          case Cleaning =>
            for
              _ <- setCountdown("battery", batteryRate)
              _ <- setCountdown("changeRoom", changeRoomRate)
            yield ()
          case GoingCharging =>
            setCountdown("changeRoom", changeRoomRate)
          case Charging => setCountdown("battery", batteryRate)

    override def onActive(e: Option[Event], timePassed: Long): FSMState[State] =
      for
        _ <- e match
          case Some(Event.ChangeMode(mode)) => setMode(mode)
          case _                            => same
        nextState <- matchCurrentState:
          _ match
            case Cleaning =>
              for
                _ <- ifCountdownReached("battery")(decBattery())
                _ <- ifCountdownReached("changeRoom")(changeRoomRandom())
                _ <- resetCountdownIfReached("battery")
                _ <- resetCountdownIfReached("changeRoom")
                battery <- inspect(_.battery)
                nextState =
                  if battery <= 10 || e == Some(Event.Stop) then GoingCharging
                  else Cleaning
              yield (nextState)
            case Charging =>
              for
                _ <- ifCountdownReached("battery")(incBattery())
                _ <- resetCountdownIfReached("battery")
                nextState =
                  if e == Some(Event.Start) then Cleaning
                  else Charging
              yield (nextState)
            case GoingCharging =>
              for
                _ <- ifCountdownReached("changeRoom")(changeRoomRandom())
                _ <- resetCountdownIfReached("changeRoom")
                newRoom <- inspect(_.currentRoom)
                chargingRoom <- inspect(_.chargingStationRoom)
                nextState =
                  if e == Some(Event.Start) then Cleaning
                  else if newRoom == chargingRoom then Charging
                  else GoingCharging
              yield (nextState)
      yield (nextState)

    private def setMode(mode: Mode): FSMState[Unit] =
      modified(_.update(mode = mode))
    private def incBattery(): FSMState[Unit] =
      modified(r => r.update(battery = r.battery + 1))
    private def decBattery(): FSMState[Unit] =
      modified(r => r.update(battery = r.battery - 1))
    private def changeRoomRandom(): FSMState[Unit] =
      modified(r => r.update(currentRoom = Random().shuffle(r.rooms).head))
