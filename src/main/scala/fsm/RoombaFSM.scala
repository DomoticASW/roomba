package fsm

object RoombaFSM:
  import scala.util.Random
  import domain.Roomba
  import domain.Roomba.*
  import domain.Roomba.State.*

  type Env = Option[Event]

  case class Model(
      r: Roomba,
      batteryRate: Long,
      batteryCountdown: Long,
      changeRoomRate: Long,
      changeRoomCountdown: Long
  )

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop

  import FSM.*
  given States[State, Model, Env] with
    def onEntry(s: State, m: Model, t: Long, e: Env): Model = m
    def onActive(
        s: State,
        m: Model,
        t: Long,
        lastActiveT: Long,
        e: Env
    ): Model = s match
      case Cleaning =>
        val reduceBattery = m.batteryCountdown <= 0
        val batteryCountdown =
          if reduceBattery then m.batteryRate
          else m.batteryCountdown - lastActiveT
        val newBattery = if reduceBattery then m.r.battery - 1 else m.r.battery

        val changeRoom = m.changeRoomCountdown <= 0
        val changeRoomCountdown =
          if changeRoom then m.changeRoomRate
          else m.changeRoomCountdown - lastActiveT
        val newRoom =
          if changeRoom then Random().shuffle(m.r.rooms).head
          else m.r.currentRoom

        m.copy(
          r = m.r.update(battery = newBattery, currentRoom = newRoom),
          batteryCountdown = batteryCountdown,
          changeRoomCountdown = changeRoomCountdown
        )
      case Charging =>
        val increaseBattery = m.batteryCountdown <= 0
        val batteryCountdown =
          if increaseBattery then m.batteryRate
          else m.batteryCountdown - lastActiveT
        val newBattery =
          if increaseBattery then m.r.battery + 1 else m.r.battery
        m.copy(
          r = m.r.update(battery = newBattery),
          batteryCountdown = batteryCountdown
        )
      case GoingCharging =>
        val changeRoom = m.changeRoomCountdown <= 0
        val changeRoomCountdown =
          if changeRoom then m.changeRoomRate
          else m.changeRoomCountdown - lastActiveT
        val newRoom =
          if changeRoom then Random().shuffle(m.r.rooms).head
          else m.r.currentRoom

        m.copy(
          r = m.r.update(currentRoom = newRoom),
          changeRoomCountdown = changeRoomCountdown
        )

    def onExit(s: State, m: Model, t: Long, e: Env): Model = m
    def nextState(
        s: State,
        m: Model,
        t: Long,
        e: Env
    ): Option[(State, Model)] = s match
      case Cleaning if m.r.battery <= 10 =>
        Some(GoingCharging, m.copy(r = m.r.update(state = GoingCharging)))
      case GoingCharging if m.r.currentRoom == m.r.chargingStationRoom =>
        Some(Charging, m.copy(r = m.r.update(state = Charging)))
      case _ => None
