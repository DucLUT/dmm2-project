package reps.dataanalysis

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.control.NonFatal
import scala.math.floor

object EnergyGenerationDataAnalysis {
  // Reads data from a CSV file, by specifying the file name and the delimiter and
  // reading the data into a buffered source and returning the lines of the data
  // as a list of arrays of strings, wrapped in an Option for error handling.
  // Function is curried to allow for partial application of the delimiter.
  //noinspection SameParameterValue
  private def readDataCsv(fileName: String)(delimiter: String): Option[List[Array[String]]] = {
    try {
      val bufferedSource = Source.fromFile(fileName)
      try Some(bufferedSource.getLines().toList.map(_.split(delimiter).map(_.trim)))
      finally bufferedSource.close()
    } catch {
      case NonFatal(_) =>
        println(s"Failed to read data from $fileName")
        None
    }
  }

  // Converts the string data into doubles, and removes the headers at headersIndex.
  //noinspection SameParameterValue
  private def extractDataCsv(data: List[Array[String]], headersIndex: Int): Option[Array[Double]] = {
    try {
      val header = data(headersIndex)
      val valuesIndex = header.indexOf("value")
      if (valuesIndex == -1) {
        println("Value column not found.")
        None

      }else {
          // Uses drop to drop the header row and map to convert the values to doubles
          // And finally converts to an array.
          Some(data.drop(headersIndex + 1).map(_(valuesIndex).toDouble).toArray)
        }
    } catch {
      case _: NumberFormatException =>
        println("Data format error.")
        None
    }
  }

  // Insertion sort algorithm using a recursive helper function to sort the data array.
  private def insertionSort(data: Array[Double]): Option[Array[Double]] = {
    try {
      insertionSortHelper(data, 0)
      Some(data)
    } catch {
      case NonFatal(_) =>
        None
    }
  }

  // Helper function to sort the data array using the insertion sort algorithm recursively.
  @tailrec
  private def insertionSortHelper(data: Array[Double], currentIndex: Int): Unit = {
    if (currentIndex < data.length) {
      insertionSortInserter(data, currentIndex)
      insertionSortHelper(data, currentIndex + 1)
    }
  }

  // Inserts the current element at the correct position in the sorted part of the array using recursion.
  @tailrec
  private def insertionSortInserter(data: Array[Double], currentIndex: Int): Unit = {
    if (currentIndex > 0 && data(currentIndex - 1) > data(currentIndex)) {
      val temp = data(currentIndex)
      data(currentIndex) = data(currentIndex - 1)
      data(currentIndex - 1) = temp
      insertionSortInserter(data, currentIndex - 1)
    }
  }

  // Calculates the statistics of the data array, including mean, median, mode, range, and midrange.
  // Returns an array of the calculated statistics, wrapped in an Option for error handling.
  // The flatMap is used to chain the calculations together, and the yield keyword is used to return the results.
  private def calculateStatistics(data: Option[Array[Double]]): Option[Array[Double]] = {
    data.flatMap { dataArray =>
      insertionSort(dataArray).flatMap { sortedData =>
        val results = for {
          meanValue <- mean(sortedData)
          medianValue <- median(sortedData)
          modeValue <- mode(sortedData)
          rangeValue <- range(sortedData)
          midrangeValue <- midrange(sortedData)
        } yield Array(meanValue, medianValue, modeValue, rangeValue, midrangeValue)
        results
      }
    }
  }

  // Returns the mean of the data array, or None if the array is empty.
  private def mean(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some(data.sum / data.length.toDouble)

  // Returns the median of the data array, or None if the array is empty.
  // Sorts the data array using the insertion sort algorithm and calculates the median.
  private def median(data: Array[Double]): Option[Double] = {
    insertionSort(data).map { sortedData =>
      sortedData(floor(sortedData.length / 2.0).toInt)
    }
  }

  // Uses the modeHelper function to calculate the mode of the data array.
  // Returns the mode of the data array, or None if the array is empty.
  private def mode(data: Array[Double]): Option[Double] = {
    if (data.isEmpty) None
    else Some(modeHelper(data, mutable.HashMap[Double, Int](), 0))
  }

  // Helper function to calculate the mode of the data array using a mutable HashMap.
  @tailrec
  private def modeHelper(data: Array[Double], elements: mutable.HashMap[Double, Int], currentIndex: Int): Double = {
    if (currentIndex >= data.length) {
      // Return the element with the highest count, or the first element if all counts are 1
      elements.maxByOption(_._2).map(_._1).getOrElse(data.head)
    } else {
      val currentElement = data(currentIndex)
      elements.updateWith(currentElement) {
        case Some(count) => Some(count + 1)
        case None => Some(1)
      }
      modeHelper(data, elements, currentIndex + 1)
    }
  }

  // Returns the range of the data array, or None if the array is empty.
  private def range(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some(data.last - data.head)

  // Returns the midrange of the data array, or None if the array is empty.
  private def midrange(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some((data.head + data.last) / 2)

  // Analyzes the energy generation data from the specified paths for solar, wind, and hydro data.
  // Returns a list of options containing the statistics for each energy generation method, wrapped in an Option for error handling.
  def analyzeData(solarDataPath: String, windDataPath: String, hydroDataPath: String): List[Option[Array[Double]]] = {
    // Partial application of curried function to read CSV data with comma delimiter.
    val readCsvWithComma = readDataCsv(_: String)(delimiter = ",")
    List(solarDataPath, windDataPath, hydroDataPath).map { path =>
      // Read the data from the CSV file, extract the data, and calculate the statistics.
      readCsvWithComma(path).flatMap { data =>
        extractDataCsv(data, 0).flatMap(dataArray => calculateStatistics(Some(dataArray)))
      }
    }
  }
}
