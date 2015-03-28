package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val sicknessDelay: Int = 6
    val deathDelay: Int = 14
    val immuneDelay: Int = 16
    val healthyDelay: Int = 18
    val deathProbability: Int = 0.25
    val prevalenceRate: Int = 0.01
    val transmissibilityRate: Int = 0.4
  }

  import SimConfig._

  val persons: List[Person] = List() // to complete: construct list of persons
  val rooms: Array.ofDim[Room](roomRows, roomColumns)
  
  def getRoom(row: Int, col: Int){   
    val rowNorm = row - (row/roomRows)*roomRows + (if(row < 0) roomRows else 0)
    val colNorm = col - (col/roomColumns)*roomColumns + (if(col < 0) roomColumns else 0)
    rooms(rowNorm)(colNorm)
  }
  
  
  class Room ( val row: Int, val col: Int){
    var personsInRoom: List[Person] = List()
    
    def add(person){
      if(personsInRoom.find(_ => _.id == person.id) == Nil) personsInRoom :+ person
    }
  
    def remove(person){
      personsInRoom = personsInRoom.filterNot(p => p.id == person.id)
    }
    
    def visibleInfected = personsInRoom.find(p => p.sick || p.dead)
    
    def infected = personsInRoom.find(p => p.infected || p.sick || p.dead || p.dead)
  
  }
  
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
  
    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    sealed abstract class Direction
    case class Up
    case class Down
    case class Left
    case class Right
    
    def getAvailableDirection() = {
        var list: List[Direction] = List()
        if(!getRoom(row + 1, col).visibleInfected) list = list :+ Up
        if(!getRoom(row - 1, col).visibleInfected) list = list :+ Down
        if(!getRoom(row, col - 1).visibleInfected) list = list :+ Left
        if(!getRoom(row, col + 1).visibleInfected) list = list :+ Right
    }
    
    def tryToMove(){
      val dirs = getAvailableDirection()
      if(!dirs.isEmpty){
        val dir = dirs(randomBelow(dirs.length))
        getRoom(row, col).remove( this )
        dir match {
          case Up => {row = (row + 1) % roomRows}
          case Down => {row = (row + 7) % roomRows}
          case Right => {col = (col + 1) % roomColumns}
          case Left => {col = (col + 7) % roomColumns}        
        }
        getRoom(row, col).add( this )
        if(getRoom(row, col).)
      }    
    }
  }
  
  
  
}
