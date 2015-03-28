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

  
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    //def tryToMove
  }
  
  
  
}
