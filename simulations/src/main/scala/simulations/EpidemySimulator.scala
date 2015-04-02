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
    val deathProbability: Double = 0.25
    val prevalenceRate: Double = 0.01
    val transmissibilityRate: Double = 0.4
  }

  import SimConfig._
  
  val prevalenceIds = for(i <- 1 to (population*prevalenceRate).toInt ) yield randomBelow(population)

  val persons: List[Person] = (for (i <- 1 to population) yield new Person(i)).toList // to complete: construct list of persons
  val rooms: Array[Array[Room]] = Array.ofDim(roomRows, roomColumns)
  for (i <- 0 until roomRows; j <- 0 until roomColumns) rooms(i)(j) = new Room(i,j)
  
  def getRoom(row: Int, col: Int): Room = {   
    val rowNorm = row - (row/roomRows)*roomRows + (if(row < 0) roomRows else 0)
    val colNorm = col - (col/roomColumns)*roomColumns + (if(col < 0) roomColumns else 0)
    rooms(rowNorm)(colNorm)
  }
  
  
  class Room ( val row: Int, val col: Int){
    var personsInRoom: List[Person] = persons.filter((p: Person) => p.row == row && p.col == col)
    
    def add(person: Person){
      if(personsInRoom.find((p: Person) => p.id == person.id) == Nil) personsInRoom :+ person
    }
  
    def remove(person: Person){
      personsInRoom = personsInRoom.filterNot((p: Person) => p.id == person.id)
    }
    
    def visibleInfected = personsInRoom.find((p: Person) => p.sick || p.dead)
    
    def infected = personsInRoom.find((p: Person) => p.infected)
  
  }
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
  
    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    if(prevalenceIds.contains(id)){
      infected = true
      afterDelay(sicknessDelay)(getSick)
    }
    prepareToMove()
    
    sealed abstract class Direction
    case class Up() extends Direction
    case class Down() extends Direction
    case class Left() extends Direction
    case class Right() extends Direction
    
    def getAvailableDirection() = {
        var list: List[Direction] = List()
        if(getRoom(row + 1, col).visibleInfected.isEmpty) list = list :+ Up()
        if(getRoom(row - 1, col).visibleInfected.isEmpty) list = list :+ Down()
        if(getRoom(row, col - 1).visibleInfected.isEmpty) list = list :+ Left()
        if(getRoom(row, col + 1).visibleInfected.isEmpty) list = list :+ Right()
        list
    }
    
    def tryToMove(){
      if (!dead) {
        val dirs = getAvailableDirection()
        if(!dirs.isEmpty){
          val dir = dirs(randomBelow(dirs.length))
          getRoom(row, col).remove( this )
          dir match {
            case _: Up => {row = (row + 1) % roomRows}
            case _: Down => {row = (row + 7) % roomRows}
            case _: Right => {col = (col + 1) % roomColumns}
            case _: Left => {col = (col + 7) % roomColumns}        
          }
          getRoom(row, col).add( this )
          if(getRoom(row, col).infected.isDefined  && 
            !infected && random < transmissibilityRate ){
            infected = true
            afterDelay(sicknessDelay)(getSick)
          }
        }
        prepareToMove()
      }
    }
    
    def prepareToMove() = {
      afterDelay(randomBelow(5)+1)(tryToMove)
    }
    
    def getSick(){
      sick = true
      if (random < deathProbability) afterDelay(deathDelay - sicknessDelay)(getDead)       
      else  afterDelay(immuneDelay - sicknessDelay)(getImmune)
    }
    
    def getDead(){
      sick = false
      dead = true
    }
    
    def getImmune(){
      sick = false
      immune = true
      afterDelay(healthyDelay - immuneDelay)(getHealthy)
    }
    
    def getHealthy(){
      immune = false
      infected = false
    }
    
  }
  
}
