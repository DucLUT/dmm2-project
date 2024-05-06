package reps.alerts

import reps.dataanalysis.EnergyGenerationDataAnalysis.analyzeData

// Duc Duong
// Mattias Slotte
// Mengshi Qi

object AlertGeneration {
  def generateAlerts(): Unit = {
    val analyzedData: List[Array[Double]] = analyzeData("data/solar.csv", "data/wind.csv", "data/hydro.csv")
  }
}
