package reps.dataanalysis

import scala.annotation.tailrec
import scala.collection.mutable

import scala.io.Source
import scala.util.control.NonFatal

// TODO: Make functions work on also other numerical types.
// TODO: Add error handling to functions
object EnergyGenerationDataAnalysis {
  private def readDataCsv(fileName: String, delimiter: String): Option[List[Array[String]]] = {
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

  //noinspection SameParameterValue
  private def extractDataCsv(data: List[Array[String]], headersIndex: Int): Option[Array[Double]] = {
    try {
      val header = data(headersIndex)
      val valuesIndex = header.indexOf("value")
      if (valuesIndex == -1) None
      else Some(data.drop(headersIndex + 1).map(_(valuesIndex).toDouble).toArray)
    } catch {
      case _: NumberFormatException =>
        println("Data format error.")
        None
    }
  }


  private def insertionSort(data: Array[Double]): Option[Array[Double]] = {
    try {
      insertionSortHelper(data, 0)
      Some(data)
    } catch {
      case NonFatal(_) =>
        None
    }
  }

  @tailrec
  private def insertionSortHelper(data: Array[Double], currentIndex: Int): Unit = {
    if (currentIndex < data.length) {
      insertionSortInserter(data, currentIndex)
      insertionSortHelper(data, currentIndex + 1)
    }
  }

  @tailrec
  private def insertionSortInserter(data: Array[Double], currentIndex: Int): Unit = {
    if (currentIndex > 0 && data(currentIndex - 1) > data(currentIndex)) {
      val temp = data(currentIndex)
      data(currentIndex) = data(currentIndex - 1)
      data(currentIndex - 1) = temp
      insertionSortInserter(data, currentIndex - 1)
    }
  }

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
    if (data.isEmpty) None else Some(data.sum / data.length)

  private def median(data: Array[Double]): Option[Double] =
    if (data.isEmpty) None else Some(data(data.length / 2))

  private def mode(data: Array[Double]): Option[Double] = {
    if (data.isEmpty) None else {
      val freqMap = mutable.Map.empty[Double, Int]
      data.foreach(num => freqMap(num) = freqMap.getOrElse(num, 0) + 1)
      Some(freqMap.maxBy(_._2)._1)
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
        extractDataCsv(data, 0).flatMap(dataArray => calculateStatistics(Some(dataArray)))  // Adjusting the call here
      }
    }
  }


}


