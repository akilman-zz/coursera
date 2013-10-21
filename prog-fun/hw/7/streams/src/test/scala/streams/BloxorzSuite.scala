package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(0,1)), "0,1")
      assert(terrain(Pos(0,2)), "0,2")
      assert(!terrain(Pos(0,3)), "0,3")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar starting state") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }
  
  test("findChar ending state") {
    new Level1 {
      assert(goal == Pos(4,7))
    }
  }
  
  test("neighborsWithHistory sample test") {
    new Level1 {
      val neighbors = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      val expected = 
        Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        )
      assert(neighbors.toSet === expected)
    }
  }
  
  test("newNeighborsOnly sample test") {
    new Level1 {
      val neighbors = Set(
    		  			(Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
		  				(Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
    		  		  ).toStream
    		  		  
      val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      
      val expected = Set(
    		  			(Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
    		  		 ).toStream
      val result = newNeighborsOnly(neighbors, explored)
      assert(result === expected)
    }
  }

  
  trait Level2 extends SolutionChecker {
	/**
	 * Right now, this is mainly tailored for testing 'for'
	 */
    val level =
    """ooooo
      |ooToo
      |ooooo
      |ooooo
      |ooSoo
      |ooooo""".stripMargin

    val optsolution = List(Up, Up)
  }
  
  test("basic for test") {
    new Level2 {
      val initial = Stream((Block(Pos(5,2), Pos(5,2)), List()))
      val explored: Set[Block] = Set.empty
      
      val path = from(initial, explored)
      
      val one = path.take(1).toList
      val two = path.take(2).toList
      val three = path.take(3).toList
      val four = path.take(4).toList
      val five = path.take(5).toList
      val six = path.take(6).toList
      val seven = path.take(7).toList
    }
  }
  
  test("simple level solve test") {
    new Level2 {
      assert(solve(solution) == Block(goal, goal))
    }
  }
  
  test("simple level solution test") {
    new Level2 {
      assert(solution.length == optsolution.length)
    }
  }
  
  trait Level0 extends SolutionChecker {
    
    // impossible solution
    val level =
      """------
        |--So-T
        |--oo--
        |--oo--
        |------""".stripMargin

    val optsolution = Nil
  }
  
  test("solution length when no solution is possible") {
    new Level0 {
      assert(solution.length == optsolution.length)
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
