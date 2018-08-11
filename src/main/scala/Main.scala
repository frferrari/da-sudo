/**
  * Created by Francois FERRARI on 11/08/2018
  */
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  val bound: Int = 9
  val subgridsBound: Int = 3

  def main(args: Array[String]) {
    for( ln <- io.Source.stdin.getLines ) {
      Try(ln.split(",").map(_.toInt)) match {
        case Success(rawValues) =>
          checkSudokuValidity(rawValues.toList)
            .map {
              case true =>
                println("True")

              case false =>
                println("False")
            }

        case Failure(_) =>
          println("Please enter a grid with integer values only")
      }
    }
  }

  /**
    * Check the sudoku grid validity based on the test/sudoku rules
    * @param rawValues
    * @return true if valid, false if invalid
    */
  def checkSudokuValidity(rawValues: Seq[Int]): Future[Boolean] = {
    if (rawValues.length != bound * bound)
      Future.successful(false)
    else {
      val sudokuRows: Seq[Seq[Int]] =
        rawValues
          .grouped(bound)
          .toList

      // Let's start all the "computations" in parallel (this computations are not CPU intensive, so it does not really matter)
      val rowsValidityF: Future[Boolean] = checkRowsValidity(sudokuRows)
      val columnsValidityF: Future[Boolean] = checkColumnsValidity(sudokuRows)
      val subgridsValidityF: Future[Boolean] = checkSubGridsValidity(sudokuRows)

      for {
        rowsValidity <- rowsValidityF
        columnsValidity <- columnsValidityF
        subgridsValidity <- subgridsValidityF
      } yield rowsValidity & columnsValidity & subgridsValidity
    }
  }

  /**
    * Check each row validity and returns a boolean, if any of the rows is not valid, the
    * whole thing is invalid
    * @param sudokuRows
    * @return true if the rows are all valide, false if any of them is invalid
    */
  def checkRowsValidity(sudokuRows: Seq[Seq[Int]]): Future[Boolean] = Future {
    sudokuRows
      .map(checkRowOrColumnValidity)
      .fold(true)(_ & _)
  }

  /**
    * Check each column validity and returns a boolean, if any of the column is not valid,
    * the whole thing is invalid
    * @param sudokuRows
    * @return true if the columns are all valid, false if any of them is invalid
    */
  def checkColumnsValidity(sudokuRows: Seq[Seq[Int]]): Future[Boolean] = Future {
    sudokuRows
      .transpose
      .map(checkRowOrColumnValidity)
      .fold(true)(_ & _)
  }

  /**
    * Check each subgrid validity and returns a boolean, if any of the subgrid is not valid,
    * the whole thing is invalid
    * @param sudokuRows
    * @return true if the subgrids aer all valid, false if any of them is invalid
    */
  def checkSubGridsValidity(sudokuRows: Seq[Seq[Int]]): Future[Boolean] = Future {
    val subgrids: Seq[Seq[Int]] =
      sudokuRows
        .map(_.grouped(subgridsBound).toList)
        .transpose
        .map(_.flatten)
        .flatMap(_.grouped(subgridsBound*subgridsBound).toList)

    subgrids
      .map(checkRowOrColumnValidity)
      .fold(true)(_ & _)
  }

  /**
    * Checks the validity of a list of integers based on Sudoku rules
    * Here I expect numbers to be in the range of 1 to 9 (I could check for this in the real like, or earlier in the code)
    * @param rowValues
    * @return
    */
  def checkRowOrColumnValidity(rowValues: Seq[Int]): Boolean =
    rowValues.size == rowValues.toSet.size
}
