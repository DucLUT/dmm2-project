package reps.dataanalysis

object EnergyGenerationDataAnalysis {
  private def readDataCsv(fileName: String, delimiter: String): List[Array[String]] = {
    val bufferedSource = io.Source.fromFile(fileName)
    val dataLines = bufferedSource.getLines().toList
    bufferedSource.close
    dataLines.map(_.split(delimiter).map(_.trim))
  }

  private def mean(data: Array[Int]): Double = {
    val length: Int = data.length
    val sum = data.sum
    sum / length
  }

  def analyzeData(): Unit = {
    val solarData: List[Array[String]] = readDataCsv("data/solar.csv", ",")
    val windData: List[Array[String]] = readDataCsv("data/wind.csv", ",")
    val hydroData: List[Array[String]] = readDataCsv("data/hydro.csv", ",")



  }


}
