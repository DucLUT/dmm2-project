package reps.dataanalysis

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.floor

// TODO: Make functions work on also other numerical types.
// TODO: Add error handling to functions
object EnergyGenerationDataAnalysis {
  //noinspection SameParameterValue
  private def readDataCsv(fileName: String, delimiter: String): List[Array[String]] = {
    val bufferedSource = io.Source.fromFile(fileName)
    val dataLines = bufferedSource.getLines().toList
    bufferedSource.close
    dataLines.map(_.split(delimiter).map(_.trim))
  }

  //noinspection SameParameterValue
  private def extractDataCsv(data: List[Array[String]], headersIndex: Int): Array[Double] = {
      val header = data(headersIndex)
      var valuesIndex = header.indexOf("value")
      if (valuesIndex == -1) valuesIndex = header.length - 1
      data.drop(headersIndex + 1).map(_(valuesIndex).toDouble).toArray
  }

  private def insertionSort(data: Array[Double]): Array[Double] = {
    insertionSortHelper(data, 0)
  }

  @tailrec
  private def insertionSortHelper(data: Array[Double], currentIndex: Int): Array[Double] = {
    if (currentIndex >= data.length) data
    else {
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

  private def mean(data: Array[Double]): Double = {
    val length: Int = data.length
    val sum = data.sum
    sum / length.toDouble
  }

  private def median(data: Array[Double]): Double = {
    val sortedData: Array[Double] = insertionSort(data)
    sortedData(floor(sortedData.length / 2.0).toInt)
  }

  private def mode(data: Array[Double]): Double = {
    modeHelper(data, mutable.HashMap[Double, Int](), 0)
  }

  @tailrec
  private def modeHelper(data: Array[Double], elements: mutable.HashMap[Double, Int], currentIndex: Int): Double = {
    if (currentIndex >= data.length) return elements.maxBy(_._2)._1

    if (elements.contains(data(currentIndex))) {
      elements.update(data(currentIndex), elements(data(currentIndex)) + 1)
      modeHelper(data, elements, currentIndex + 1)
    } else {
      elements += (data(currentIndex) -> 1)
      modeHelper(data, elements, currentIndex + 1)
    }
  }

  private def range(data: Array[Double]): Double = {
    val sortedData: Array[Double] = insertionSort(data)
/*    print("sortedData: ")
    println(sortedData.mkString(", "))*/
    sortedData.last - sortedData(0)
  }

  private def midrange(data: Array[Double]): Double = {
    val sortedData: Array[Double] = insertionSort(data)
    (sortedData(0) + sortedData.last) / 2.0
  }

  private def getStatistics(data: Array[Double]): Array[Double] = {
    val dataMean: Double = mean(data)
    val dataMedian: Double = median(data)
    val dataMode: Double = mode(data)
    val dataRange: Double = range(data)
    val dataMidrange: Double = midrange(data)

    val statistics: Array[Double] = Array(dataMean, dataMedian, dataMode, dataRange, dataMidrange)
    statistics.map(stat => BigDecimal(stat).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
  }

  def analyzeData(solarDataPath: String, windDataPath: String, hydroDataPath: String): List[Array[Double]] = {
    val solarData: List[Array[String]] = readDataCsv(solarDataPath, ",")
    val windData: List[Array[String]] = readDataCsv(windDataPath, ",")
    val hydroData: List[Array[String]] = readDataCsv(hydroDataPath, ",")

    val solarValues: Array[Double] = extractDataCsv(solarData, 0)
    val solarStatistics: Array[Double] = getStatistics(solarValues)

    val windValues: Array[Double] = extractDataCsv(windData, 0)
    val windStatistics: Array[Double] = getStatistics(windValues)

    val hydroValues: Array[Double] = extractDataCsv(hydroData, 0)
    val hydroStatistics: Array[Double] = getStatistics(hydroValues)

    List(solarStatistics, windStatistics, hydroStatistics)
  }
}
