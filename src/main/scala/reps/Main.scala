package reps

import reps.datacollection.EnergyGenerationDataCollection.fetchEnergyData
import reps.views.PowerPlantView.choice
// Duc Duong
// Mattias Slotte
// Mengshi Qi

object Main {
  def menu(): Unit = {
    println("1. View Power Plant Data")
    println("2. Analyze Energy Generation Data")
    println("3. Generate Alerts")
    println("4. Exit")
    print("Enter your choice: ")
  }

  def executeOption(option: Int): Unit = {
    option match {
      case 1 => choice()
      case _ => println("Invalid choice")
    }
  }
  def main(args: Array[String]): Unit = {

    fetchEnergyData("https://data.fingrid.fi/api/datasets/191/data", "hydro.csv")
    fetchEnergyData("https://data.fingrid.fi/api/datasets/248/data", "solar.csv")
    fetchEnergyData("https://data.fingrid.fi/api/datasets/75/data", "wind.csv")

    var continue = true
    while (continue) {
      menu()
      val option = scala.io.StdIn.readInt()
      if (option == 4) {
        continue = false
      } else {
        executeOption(option)
      }
    }


  }
}
