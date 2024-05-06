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
  //noinspection SameParameterValue
  private def readDataCsv(fileName: String, delimiter: String): Option[List[Array[String]]] = {
    try {
      val bufferedSource = Source.fromFile(fileName)
      // Extract lines, convert to list and split by delimiter and trim the values using maps.
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

  //
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

  private def mean(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some(data.sum / data.length.toDouble)

  // TODO: Redo error handling on median without changing its format
  private def median(data: Array[Double]): Option[Double] = {
    insertionSort(data).map { sortedData =>
      sortedData(floor(sortedData.length / 2.0).toInt)
    }
  }

  // TODO: Redo error handling on mode and modeHelper without changing their format
  private def mode(data: Array[Double]): Option[Double] = {
    if (data.isEmpty) None
    else Some(modeHelper(data, mutable.HashMap[Double, Int](), 0))
  }

  @tailrec
  private def modeHelper(data: Array[Double], elements: mutable.HashMap[Double, Int], currentIndex: Int): Double = {
    if (currentIndex >= data.length) {
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

  private def range(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some(data.last - data.head)

  private def midrange(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some((data.head + data.last) / 2)

  // Public method to analyze data, handling file read and processing safely
  def analyzeData(solarDataPath: String, windDataPath: String, hydroDataPath: String): List[Option[Array[Double]]] = {
    List(solarDataPath, windDataPath, hydroDataPath).map { path =>
      readDataCsv(path, ",").flatMap { data =>
        extractDataCsv(data, 0).flatMap(dataArray => calculateStatistics(Some(dataArray)))
      }
    }
  }
}
