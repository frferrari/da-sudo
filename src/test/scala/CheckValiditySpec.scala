import org.scalatest.AsyncWordSpec

/**
  * Created by Francois FERRARI on 11/08/2018
  */
class CheckValiditySpec extends AsyncWordSpec {
  val validGrid = List(
    List(1, 2, 3, 4, 5, 6, 7, 8, 9),
    List(4, 5, 6, 7, 8, 9, 1, 2, 3),
    List(7, 8, 9, 1, 2, 3, 4, 5, 6),
    //
    List(2, 3, 4, 5, 6, 7, 8, 9, 1),
    List(5, 6, 7, 8, 9, 1, 2, 3, 4),
    List(8, 9, 1, 2, 3, 4, 5, 6 ,7),
    //
    List(3, 4, 5, 6, 7, 8, 9, 1, 2),
    List(6, 7, 8, 9, 1, 2, 3, 4, 5),
    List(9, 1, 2, 3, 4, 5, 6, 7, 8)
  )

  val invalidGridByRow: Seq[Seq[Int]] = List(
    List(1, 2, 3, 4, 5, 6, 7, 8, 9),
    List(4, 5, 4, 7, 8, 9, 1, 2, 3), // This row is invalid on col 3
    List(7, 8, 9, 1, 2, 3, 4, 5, 6),
    //
    List(2, 3, 4, 5, 6, 7, 8, 9, 1),
    List(5, 6, 7, 8, 9, 1, 2, 3, 4),
    List(8, 9, 1, 2, 3, 4, 5, 6 ,7),
    //
    List(3, 4, 5, 6, 7, 8, 9, 1, 2),
    List(6, 7, 8, 9, 1, 2, 3, 4, 5),
    List(9, 1, 2, 3, 4, 5, 6, 7, 8)
  )

  val invalidGridByColumn: Seq[Seq[Int]] = List(
    List(1, 2, 3, 4, 5, 6, 7, 8, 9),
    List(4, 5, 6, 7, 8, 9, 1, 2, 3),
    List(7, 8, 9, 1, 2, 3, 4, 5, 6),
    //
    List(4, 3, 2, 5, 6, 7, 8, 9, 1), // This row has invalid columns 1 and 3
    List(5, 6, 7, 8, 9, 1, 2, 3, 4),
    List(8, 9, 1, 2, 3, 4, 5, 6 ,7),
    //
    List(3, 4, 5, 6, 7, 8, 9, 1, 2),
    List(6, 7, 8, 9, 1, 2, 3, 4, 5),
    List(9, 1, 2, 3, 4, 5, 6, 7, 8)
  )

  val invalidGridBySubgrid = List(
    List(1, 2, 3, 4, 5, 6, 7, 8, 9),
    List(4, 5, 6, 7, 8, 9, 1, 2, 3),
    List(7, 8, 9, 1, 2, 3, 4, 5, 6),
    //
    List(2, 3, 4, 5, 6, 7, 8, 9, 1),
    List(5, 6, 7, 8, 9, 1, 2, 3, 4),
    List(8, 9, 1, 2, 3, 4, 5, 6 ,7),
    //
    List(3, 4, 5, 6, 7, 8, 9, 1, 2),
    List(6, 7, 8, 9, 1, 2, 3, 4, 5),
    List(9, 1, 3, 3, 4, 5, 6, 7, 8) // This row is invalid on col 3 (subgrid with digit 3 appearing twice)
  )

  "checkRowOrColumnValidity" should {
    "return true for a valid list of integers" in {
      Main.checkRowOrColumnValidity(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) map { validity => assert(validity) }
    }
    "return false for an invalid list of integers" in {
      Main.checkRowOrColumnValidity(List(1, 2, 3, 4, 5, 6, 7, 8, 1)) map { validity => assert(!validity) }
    }
  }

  "checkRowsValidity" should {
    "return true for a valid grid" in {
      Main.checkRowsValidity(validGrid) map { validity => assert(validity) }
    }
    "return false for an invalid grid" in {
      Main.checkRowsValidity(invalidGridByRow) map { validity => assert(!validity) }
    }
  }

  "checkColumnsValidity" should {
    "return true for a valid grid" in {
      Main.checkColumnsValidity(validGrid) map { validity => assert(validity) }
    }
    "return false for an invalid grid" in {
      Main.checkColumnsValidity(invalidGridByColumn) map { validity => assert(!validity) }
    }
  }

  "checkSubgridsValidity" should {
    "return true for a valid grid" in {
      Main.checkSubGridsValidity(validGrid) map { validity => assert(validity) }
    }
    "return false for an invalid grid" in {
      Main.checkSubGridsValidity(invalidGridBySubgrid) map { validity => assert(!validity) }
    }
  }

  "checkSudokuValidity" should {
    "return true for a valid grid" in {
      Main.checkSudokuValidity(validGrid.flatten) map { validity => assert(validity) }
    }
    "return false for an invalid grid by row" in {
      Main.checkSudokuValidity(invalidGridByRow.flatten) map { validity => assert(!validity) }
    }
    "return false for an invalid grid by column" in {
      Main.checkSudokuValidity(invalidGridByColumn.flatten) map { validity => assert(!validity) }
    }
    "return false for an invalid grid by number of elements in the grid less than required" in {
      Main.checkSudokuValidity(Nil) map { validity => assert(!validity) }
    }
    "return false for an invalid grid by number of elements in the grid greater than required" in {
      Main.checkSudokuValidity((1 to Main.bound*Main.bound+1).toList) map { validity => assert(!validity) }
    }
  }
}
