package reps

import reps.datacollection.EnergyGenerationDataCollection.fetchEnergyData
import reps.views.PowerPlantView.choice
// Duc Duong
// Mattias Slotte
// Mengshi Qi

object Main {
  def main(args: Array[String]): Unit = {

    fetchEnergyData("https://data.fingrid.fi/api/datasets/191/data", "hydro.csv")
    fetchEnergyData("https://data.fingrid.fi/api/datasets/248/data", "solar.csv")
    fetchEnergyData("https://data.fingrid.fi/api/datasets/75/data", "wind.csv")

    choice()
  }
}
