package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    
    val infectionRate = 0.4
    val prevalenceRate = 0.01
    val deathRate = 0.25
    
    val incubationDelay = 6
    val deathDelay = 14
    val immunizationDelay = 16
    val returnToHealthyDelay = 18
    
    val airTrafficRate = 0.01
    val vacciantionRate = 0.05
    
    val enableAirTraffic = false
    val enableReduceMobilityAct = false
    val enableChosenFewAct = false
  }

  import SimConfig._

  val persons: List[Person] = createPersonList
  var nPersons = 0
  
  /**
   * Returns the next id for a person
   */
  def nextId = {
    val id = nPersons
    nPersons = nPersons + 1
    id
  }
  
  /**
   * Creates a list of people for the simulation
   * 
   * In order to maintain the prevalence rate, this approach calculates the number of
   * people who should be infected; rather than using the biased coin appraoch as the
   * test case expects a static number of infections
   */
  def createPersonList:List[Person] = {
    def loop(n: Int, f: Int => Person):List[Person] = {
      n match {
        case 0 => Nil
        case _ => f(nextId) :: loop(n - 1, f)
      }
    }
    
    val healthyPeople:List[Person] = loop(population, id => new Person(id))
    
    var toInfect = prevalenceRate * population
    var toVaccinate = vacciantionRate * population
    
    healthyPeople.foreach(p => {
      
      // infect the first few to match prevalence rate
      if (toInfect != 0) {
        p.infect
        toInfect = toInfect - 1
      }
      
      if (enableChosenFewAct && toVaccinate != 0) {
        p.immune = true
        toVaccinate = toVaccinate - 1
      }
      
      // schedule moves for each
      p.scheduleMove
    })
    
    scala.util.Random.shuffle(healthyPeople)
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    //
    // to complete with simulation logic
    //
    
    def move {
      
      if (!dead) {
        
        if (enableAirTraffic && flyToNewLocation) {
          row = randomBelow(roomRows)
          col = randomBelow(roomColumns)
        } else {
          val possibleMoves = Direction.getShuffledMoveList(row, col)
          val safeMoves = possibleMoves.filter { case (_, (row, col)) => isSafe(row, col) }

          safeMoves match {
            case Nil => ()
            case moveTuple :: _ => {

              val (_, (newRow, newCol)) = moveTuple
              row = newRow
              col = newCol
            }
          }
        }

        if (infected || sick) {
          contaminate(row, col)
        } else {
          if (isContagious(row, col) && random <= infectionRate) {
            infect
          }
        }
        
        scheduleMove
      }
    }
    
    def flyToNewLocation = random < airTrafficRate 

    /**
     * Move scheduled from 1 to 5 days with equally distributed probability
     */
    def scheduleMove {
      val delay = 1 + randomBelow(4)
      if (enableReduceMobilityAct) {
    	if (sick)
    	  afterDelay(delay*4) { move }
    	else 
    	  afterDelay(delay*2) { move }
      } else {
        afterDelay(delay) { move }
      }
    }
    
    /**
     * Infects 'this' person with Scaliosis, and queues other defined actions
     */
    def infect = {
      infected = true
      
      afterDelay(incubationDelay) {
        if (!immune && !sick){
          sick = true	
        }
      }
      
      afterDelay(deathDelay) {  
        if (sick && !dead && random <= deathRate) {
          dead = true
        }
      }
      
      afterDelay(immunizationDelay) {
        if (sick && !dead) {
          immune = true
          sick = false
        }
      }
      
      afterDelay(returnToHealthyDelay) {
        if (!dead) {
          infected = false;
          sick = false;
          immune = false;
        }
      }
    }

    /**
     * Returns a boolean indicating if a person is at a given coordinate
     */
    def isAt(row: Int, col: Int) = {
      this.row == row && this.col == col
    }
    
  } // class Person
  
  /**
   * Checks if a given location satisfies some predicate function 'f'
   */
  def checkLocation(row: Int, col: Int, f: Person => Boolean) = {

    def loop(persons: List[Person]):Boolean = {
      persons match {
        case Nil => false
        case p :: ps => {
          if (p.isAt(row, col) && f(p)) true
          else loop(ps)
        }
      }
    }
    
    loop(persons)
  }
  
  /**
   * Contaminates a location. To be precise, iterates through each person at a 
   * given coordinate and infects based on the infection probability rate
   */
  def contaminate(row: Int, col: Int):Unit = {
    applyToLocation(row, col, p => if (random <= infectionRate) p.infect)
  }
  
  /**
   * Apply function 'f' to all people in a given location
   */
  def applyToLocation(row: Int, col: Int, f: Person => Unit) = {
    persons.filter(p => row == p.row && col == p.col).foreach(f)
  }
  
  /**
   * Location is transmissible if it contains people who are sick, dead, or infected
   */
  def isContagious(row: Int, col: Int) = {
    checkLocation(row, col, p => p.sick || p.dead || p.infected)
  }
  
  /**
   * Location is 'safe' if coordinates do not contain visibly infected people, defined
   * as 'sick' or 'dead'
   */
  def isSafe(row: Int, col: Int) = {
    ! checkLocation(row, col, p => p.sick || p.dead)
  }

  /**
   * Enumeration representing directions a person can move
   */
  object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
    val NumDirections = 4

    /**
     * Given a pair of coordinates and a direction, returns a tuple representing the
     * new coordinates
     */
    def getCoordinates(coordinates: (Int, Int), move: Direction) = {
      val row = coordinates._1
      val col = coordinates._2

      move match {
        case Up => (row, (col - 1 + roomColumns) % roomColumns)
        case Down => (row, (col + 1) % roomColumns)
        case Left => ((row - 1 + roomRows) % roomRows, col)
        case Right => ((row + 1) % roomRows, col)
      }
    }

    /**
     * Utility method to return a shuffled list of moves with coordinates
     */
    def getShuffledMoveList(row: Int, col: Int) = {
      val moves = Up :: Down :: Left :: Right :: Nil
      val shuffledMoves = scala.util.Random.shuffle(moves)
      shuffledMoves.map(move => (move, getCoordinates((row, col), move)))
    }
  }

}
