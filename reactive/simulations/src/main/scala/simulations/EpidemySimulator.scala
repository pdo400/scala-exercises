package simulations

import math.random
import scala.annotation.tailrec

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def mod(m: Int)(x: Int) = (x % m + m) % m

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation

    val prevalenceRate = 0.01
    val transmissionRate = 0.4
    val mortalityRate = 0.25

    val toSickDelay = 6
    val toMaybeDeadDelay = 14
    val toImmuneDelay = 16
    val toHealthyDelay = 18

    val standardMobility = 5

    val reducedMobility = true
    val reducedMobilityClean = 2 * standardMobility
    val reducedMobilityVisiblyInfected = 2 * reducedMobilityClean

    val chosenFew = true
    val chosenFewVaccinationRate = 0.05

    val airTraffic = true
    val airTrafficRate = 0.01
  }

  import SimConfig._

  val persons: List[Person] = ((0 until population).view map { new Person(_) }).toList

  def inhabitants(room: Room) = persons.view filter { _.room == room }

  def moveDelay(p: Person) = {
    val mobility =
      if (reducedMobility)
        if (p.visiblyInfectious) reducedMobilityVisiblyInfected else reducedMobilityClean
      else
        standardMobility

    randomBelow(mobility) + 1
  }

  def randomMove(room: Room) = {
    if (airTraffic && random < airTrafficRate) {
      Some(Room.randomNot(room))
    } else {
      val potential = Room.getConnected(room) .
        view . filter { inhabitants(_) forall { !_.visiblyInfectious } }
      val size = potential.size
      if (size == 0) None else Some(potential(randomBelow(size)))
    }
  }

  object Room {
    private[this] val rooms = new Array[Room](roomRows * roomColumns)
    private[this] val connected = new Array[Array[Room]](roomRows * roomColumns)
    private[this] val toRow = mod(roomRows)_
    private[this] val toCol = mod(roomColumns)_
    private[this] def indexOf(row: Int, col: Int): Int = roomColumns * toRow(row) + toCol(col)
    private[this] def indexOf(room: Room): Int = indexOf(room.row, room.col)

    for {
      row <- 0 until roomRows
      col <- 0 until roomColumns
      room = Room(row, col)
    } {
      rooms(indexOf(room)) = room
    }

    for (room <- rooms) {
      connected(indexOf(room)) = Array(
        get(room.row + 1, room.col),
        get(room.row - 1, room.col),
        get(room.row, room.col + 1),
        get(room.row, room.col - 1))
    }

    def get(row: Int, col: Int) = rooms(indexOf(row, col))

    def getConnected(room: Room) = connected(indexOf(room))

    def random = rooms(randomBelow(rooms.size))

    def randomNot(room: Room) = {
      val r = randomBelow(rooms.size - 1)
      val i = if (r < indexOf(room)) r else r + 1
      rooms(i)
    }
  }

  case class Room (row: Int, col: Int)

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var room: Room = Room.random

    def row = room.row
    def col = room.col

    def visiblyInfectious = sick || dead

    //
    // to complete with simulation logic
    //

    if (id % (1 / prevalenceRate).toInt == 0)
      becomeInfected()
    else if (chosenFew && random < chosenFewVaccinationRate)
      immune = true

    queueMove()

    def queueMove() = afterDelay(moveDelay(this)) { move() }

    def move() {
      if (!dead) {
        randomMove(room) match {
          case Some(moveTo) =>
            room = moveTo
            if (!infected && !immune && random < transmissionRate && inhabitants(moveTo).exists(_.infected))
              becomeInfected()
          case None =>
        }
        queueMove()
      }
    }

    def becomeInfected() {
      infected = true
      afterDelay(toSickDelay) { becomeSick() }
    }

    def becomeSick() {
      sick = true
      afterDelay(toMaybeDeadDelay - toSickDelay) { becomeMaybeDead() }
    }

    def becomeMaybeDead() {
      if (dead || random < mortalityRate)
        // check if already dead due to test that directly mutates state outside of the simulation event cycle
        dead = true
      else
        afterDelay(toImmuneDelay - toMaybeDeadDelay) { becomeImmune() }
    }

    def becomeImmune() {
      immune = true
      sick = false
      afterDelay(toHealthyDelay - toImmuneDelay) { becomeHealthy() }
    }

    def becomeHealthy() {
      infected = false
      immune = false
    }
  }
}
