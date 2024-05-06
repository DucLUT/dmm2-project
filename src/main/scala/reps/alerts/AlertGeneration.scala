package reps.alerts

import reps.dataanalysis.EnergyGenerationDataAnalysis.analyzeData
import reps.control.RenewableControl.readPlantStatusFromFile

// Duc Duong
// Mattias Slotte
// Mengshi Qi

object AlertGeneration {
  def generateAlerts(): Unit = {
    val analyzedData: List[Option[Array[Double]]] = analyzeData("data/solar.csv", "data/wind.csv", "data/hydro.csv")
    val alertData: List[Array[Double]] = analyzedData.flatten

    if (alertData.size < 3) {
      println("Error: Insufficient data to generate alerts.")
      return
    }

    val solarData: Array[Double] = alertData.head
    val windData: Array[Double] = alertData(1)
    val hydroData: Array[Double] = alertData(2)

    val solarAverageThreshold: Double = 100
    val windAverageThreshHold: Double = 800
    val hydroAverageThreshHold: Double = 1000

    // Alert if the average energy generation is below the normal expected average
    if (solarData(0) < solarAverageThreshold) println("Alert: Average solar energy generation is below 100.")
    if (windData(0) < windAverageThreshHold) println("Alert: Average wind energy generation is below 800.")
    if (hydroData(0) < hydroAverageThreshHold) println("Alert: Average hydro energy generation is below 1000.")

    // Alert if the rate of change of energy generation is negative and above a certain threshold
    val changeOfRateThreshold: Double = -0.1

    val solarRateOfChange: Double = (solarData(0) - solarData(1)) / solarData(1)
    val windRateOfChange: Double = (windData(0) - windData(1)) / windData(1)
    val hydroRateOfChange: Double = (hydroData(0) - hydroData(1)) / hydroData(1)

    if (solarRateOfChange < 0.0 && solarRateOfChange < changeOfRateThreshold) println("Alert: Solar energy generation rate of change is negative and above " + changeOfRateThreshold + ".")
    if (windRateOfChange < 0.0 && windRateOfChange < changeOfRateThreshold) println("Alert: Wind energy generation rate of change is negative and above " + changeOfRateThreshold + ".")
    if (hydroRateOfChange < 0.0 && hydroRateOfChange < changeOfRateThreshold) println("Alert: Hydro energy generation rate of change is negative and above " + changeOfRateThreshold + ".")

    // Alert if any of the renewable plants are not running
    val plantStatuses: Option[(Boolean, Boolean, Boolean)] = readPlantStatusFromFile()
    plantStatuses match {
      case Some((solarStatus, windStatus, hydroStatus)) =>
        if (!solarStatus) println("Alert: Solar plant is not running.")
        if (!windStatus) println("Alert: Wind plant is not running.")
        if (!hydroStatus) println("Alert: Hydro plant is not running.")
      case None => println("Error: Unable to read plant statuses from file.")
    }
  }
}