package domain

import fsm.FSM.*
import ports.Server
import Roomba.*

/** @param roomba
  *   The initial state of the roomba
  * @param periodMs
  *   It is suggested to choose a period which is less than the MEST (Minimum
  *   Event Separation Time)
  */
class RoombaAgent(private var _roomba: Roomba, periodMs: Long) extends Thread:

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

  private var server: Option[Server] = None

  /** Once registered to a server the agent will send it's state once every
    * `periodMs`
    */
  def registerToServer(server: Server): Unit =
    synchronized:
      this.server = Some(server)

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
      server.foreach(_.sendCurrentState(roomba))
