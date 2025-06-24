package domain

import fsm.FSM.*
import ports.ServerCommunicationProtocol.*
import Roomba.*

/** @param roomba
  *   The initial state of the roomba
  * @param periodMs
  *   It is suggested to choose a period which is less than the MEST (Minimum
  *   Event Separation Time)
  */
class RoombaAgent(
    val serverComm: ServerCommunicationProtocol,
    private var _roomba: Roomba,
    private val periodMs: Long,
    private val announceEveryMs: Long
) extends Thread:

  def roomba: Roomba = synchronized { _roomba }
  def roomba_=(r: Roomba): Unit = synchronized { _roomba = r }

  private var events: Seq[Event] = Seq()

  def enqueEvent(e: Event): Unit =
    synchronized:
      events = events :+ e

  private def takeEvents(): Seq[Event] =
    synchronized:
      val res = events
      events = Seq()
      res

  private var serverAddress: Option[ServerAddress] = None
  private var timeFromLastAnnounceMs: Long = 0

  /** Once registered to a server the agent will send it's state once every
    * `periodMs`
    */
  def registerToServer(serverAddress: ServerAddress): Unit =
    synchronized:
      this.serverAddress = Some(serverAddress)

  private var _shouldStop = false
  private def shouldStop: Boolean = synchronized { _shouldStop }
  def setShouldStop(): Unit = synchronized { _shouldStop = true }
  override def run(): Unit =
    while !shouldStop do
      Thread.sleep(periodMs)
      val events = takeEvents()
      roomba = events match
        case h :: t =>
          val r = roomba.step(periodMs, Some(h))
          t.foldLeft(r)((r, e) => r.step(0, Some(e)))
        case Nil => roomba.step(periodMs, None)
      serverAddress match
        case Some(serverAddress) =>
          serverComm.sendCurrentState(serverAddress, roomba)
        case None if timeFromLastAnnounceMs >= announceEveryMs =>
          serverComm.announce(roomba)
          timeFromLastAnnounceMs = 0
        case None =>
          timeFromLastAnnounceMs += periodMs
