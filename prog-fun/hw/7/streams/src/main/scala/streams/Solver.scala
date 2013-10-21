package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   * 
   * Logic: b1 and implicitly b2 must be at the goal. Furthermore, 
   * the block must be standing. Since, by standing b1 must be at
   * the same x,y coordinates as b2; only b1 being at the goal
   * is checked
   */
  def done(b: Block): Boolean = b.b1 == goal && b.isStanding

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    b.legalNeighbors.map(e => (e._1, e._2 :: history)).toStream
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    neighbors.filter(e => !explored.contains(e._1))
  }

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    
    if (initial.isEmpty) Stream.empty
    else {
      val headBlock = initial.head._1
      val headMoves = initial.head._2
      
      val neighbors = neighborsWithHistory(headBlock, headMoves)	// get neighbors based of head of initial stream 
      val newNeighbors = newNeighborsOnly(neighbors, explored)		// filter out explored blocks
      
      
      val updatedPathsStream = initial.tail ++ newNeighbors			// next set of blocks to explore are new neighbors
      																// invariant: path in ascending length of moves	
      
      val updatedExploredSet =  explored + initial.head._1			// we've now explored 'head' of initial list
      
      initial.head #:: from(updatedPathsStream, updatedExploredSet) // effectively, this results in a BFS
    }
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {
    
    val startingBlock = Block(startPos, startPos)			// standing, both at startPos
    val movesSoFar = List()									// starting fresh, no history
    val explored: Set[Block] = Set.empty					// nothing yet explored
    
    from(Stream((startingBlock, movesSoFar)), explored)
  }

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] =  
    pathsFromStart.filter(e => e._1 == Block(goal, goal))

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   * 
   * Logic: Paths to goal has multiple paths to the goal state. Assuming the path is constructed correctly, the head
   * of the list should have the shortest path. If so, the head should be a pair of (Block(goal, goal), listOfMoves)
   * where the list of moves 'head' has the last move played to get to the goal. The solution s.b. the reverse of this
   */
  lazy val solution: List[Move] = {
    if (pathsToGoal.size == 0) Nil
    else {
		val firstPair = pathsToGoal.head
		val moves = firstPair._2
		val result = moves.reverse
		result
    }
  }
}
