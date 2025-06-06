package domain
import scala.util.Random
import fsm.FSM
import domain.Roomba.*
import domain.Roomba.State.*

object RoombaFSM:

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop

  object Countdowns:
    val battery = "battery"
    val changeRoom = "changeRoom"

  import FSM.*

  given FSMState[State, Roomba, Event] with

    def currentState: FSMState[State] = inspect(_.state)
    def setCurrentState(s: State): FSMState[Unit] =
      modified(r => r.update(state = s))
    override def onEntry(): FSMState[Unit] =
      matchCurrentState:
        _ match
          case Cleaning =>
            for
              _ <- setBatteryCountdown
              _ <- setChangeRoomCountdown
            yield ()
          case GoingCharging => setChangeRoomCountdown
          case Charging      => setBatteryCountdown

    override def onActive(e: Option[Event], timePassed: Long): FSMState[State] =
      for
        _ <- e match
          case Some(Event.ChangeMode(mode)) => setMode(mode)
          case _                            => same
        nextState <- matchCurrentState:
          _ match
            case Cleaning =>
              for
                _ <- ifCountdownReached(Countdowns.battery)(decBattery())
                _ <- ifCountdownReached(Countdowns.changeRoom)(
                  changeRoomRandom()
                )
                _ <- resetCountdownIfReached(Countdowns.battery)
                _ <- resetCountdownIfReached(Countdowns.changeRoom)
                battery <- inspect(_.battery)
                nextState =
                  if battery <= 10 || e == Some(Event.Stop) then GoingCharging
                  else Cleaning
              yield (nextState)
            case Charging =>
              for
                _ <- ifCountdownReached(Countdowns.battery)(incBattery())
                _ <- resetCountdownIfReached(Countdowns.battery)
                nextState =
                  if e == Some(Event.Start) then Cleaning
                  else Charging
              yield (nextState)
            case GoingCharging =>
              for
                _ <- ifCountdownReached(Countdowns.changeRoom)(
                  changeRoomRandom()
                )
                _ <- resetCountdownIfReached(Countdowns.changeRoom)
                newRoom <- inspect(_.currentRoom)
                chargingRoom <- inspect(_.chargingStationRoom)
                nextState =
                  if e == Some(Event.Start) then Cleaning
                  else if newRoom == chargingRoom then Charging
                  else GoingCharging
              yield (nextState)
      yield (nextState)

    private def setBatteryCountdown: FSMState[Unit] =
      for
        ms <- inspect(_.batteryRateMs)
        _ <- setCountdown(Countdowns.battery, ms)
      yield ()
    private def setChangeRoomCountdown: FSMState[Unit] =
      for
        ms <- inspect(_.changeRoomRateMs)
        _ <- setCountdown(Countdowns.changeRoom, ms)
      yield ()
    private def setMode(mode: Mode): FSMState[Unit] =
      modified(_.update(mode = mode))
    private def incBattery(): FSMState[Unit] =
      modified(r => r.update(battery = r.battery + 1))
    private def decBattery(): FSMState[Unit] =
      modified(r => r.update(battery = r.battery - 1))
    private def changeRoomRandom(): FSMState[Unit] =
      modified(r => r.update(currentRoom = Random().shuffle(r.rooms).head))
