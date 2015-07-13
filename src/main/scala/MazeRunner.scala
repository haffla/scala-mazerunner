import java.io.File
import pl.project13.scala.rainbow._

object MazeRunner {

  case class Cell(free: Boolean, start: Boolean)

  case class Position(column: Int, row: Int) {
    override def toString = "(" + column + " | " + row + ")"
    def north = Position(column - 1, row)
    def south = Position(column + 1, row)
    def west = Position(column, row - 1)
    def east = Position(column, row + 1)
  }

  def isExit(pos: Position, lab: Maze): Boolean = {
    val cell = lab(pos.column)(pos.row)
    cell.free && (pos.column == 0
      || pos.column == lab.length - 1
      || pos.row == 0
      || pos.row == lab.head.length - 1)
  }

  type Maze = Seq[Seq[Cell]]

  def main(args: Array[String]) = {
    val maze = createMaze(new File(".").getCanonicalPath + "/src/main/resources/maze.txt")
    if(maze.nonEmpty) {
      val entrance = findStart(maze, Position(0, 0), List(Position(0,0)))
      if(entrance.nonEmpty) {
        val result = findExit(maze, entrance.head, entrance)
        result match {
          case Some(x) => printSolution(maze, x)
          case None => println("No exit found. You are trapped.")
        }
      }
      else println("No entrance found!")

    } else println("No maze found!")
  }

  def printSolution(maze:Maze, sol: List[Position]):Unit = {
    for (a <- maze.indices) {
      for (b <- maze(a).indices) {
        if(sol.contains(Position(a,b)) && !maze(a)(b).start) print {"*".green}
        else {
          if(maze(a)(b).free) print(" ")
            else if(maze(a)(b).start) print {"?".red}
          else print("#")
        }
      }
      print("\n")
    }
  }

  def findStart(maze: Maze, position: Position, visited: List[Position]): List[Position] = {
    if (maze(position.column)(position.row).start) {
      List(position)
    }
    else {
      val nextPositions: List[Position] = List(position.north, position.east, position.south, position.west) filter (x =>
        !visited.contains(x) && isAccessible(maze, x))
      nextPositions.flatMap { notYetVisitedPosition =>
        findStart(maze, notYetVisitedPosition, position :: visited)
      }
    }
  }

  private def isAccessible(maze: Maze, position: Position): Boolean =
    indexInBoundaries(maze, position) &&
      ( maze(position.column)(position.row).free || maze(position.column)(position.row).start )

  private def indexInBoundaries(maze: Maze, position: Position): Boolean =
    position.column >= 0 && position.row >= 0 && position.column < maze.size && position.row < maze(position.column).size

  def findExit(maze: Maze, pos: Position, walkedSoFar: List[Position]): Option[List[Position]] = {
    if (isExit(pos, maze)) Some(pos :: walkedSoFar)
    else {
      val posList = List(pos.north, pos.south, pos.west, pos.east).filter(x => isAccessible(maze, x) && !walkedSoFar.contains(x))
      val ways = posList.flatMap { x =>
        findExit(maze, x, x::walkedSoFar)
      }
      getSolution(maze, ways)
    }
  }

  def getSolution(maze:Maze, ways: List[List[Position]]):Option[List[Position]] = {
    val winner = ways.filter(way => isExit(way.head, maze))
    winner.headOption
  }

  def createMaze(filePath: String): Maze = {
    try {
      (for (column <- io.Source.fromFile(filePath, "UTF-8").getLines().toList)
        yield for (cell <- column) yield Cell(cell == ' ', cell == '$')).toIndexedSeq
    } catch { case _: java.io.FileNotFoundException => Nil }
  }
}