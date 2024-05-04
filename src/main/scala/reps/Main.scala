package reps

import reps.datacollection.EnergyGenerationDataCollection.fetchEnergyData
import reps.views.PowerPlantView.choice
import reps.dataanalysis.EnergyGenerationDataAnalysis.analyzeData

import scala.annotation.tailrec

//Duc Duong
//Mattias Slotte
//Mengshi Qi

// Define a sealed trait to represent the menu options
sealed trait MenuOption
object MenuOption {
  case object ViewPowerPlantData extends MenuOption
  case object AnalyzeEnergyGenerationData extends MenuOption
  case object GenerateAlerts extends MenuOption
  case object Exit extends MenuOption
}

object Main {
  import MenuOption._

  // Function to display the menu options
  private def displayMenu(): Unit = {
    println("1. View Power Plant Data")
    println("2. Analyze Energy Generation Data")
    println("3. Generate Alerts")
    println("4. Exit")
    print("Enter your choice: ")
  }

  // Function to execute the selected menu option
  private def executeOption(option: MenuOption): Unit = option match {
    case ViewPowerPlantData => choice()
    case AnalyzeEnergyGenerationData => println(analyzeData("data/solar.csv", "data/wind.csv", "data/hydro.csv"))
    case GenerateAlerts => println("Generate Alerts")
    case Exit => println("Exiting...")
  }

  // Function to get user input as a menu option
  @tailrec
  private def getUserChoice: MenuOption = {
    val choice = scala.io.StdIn.readInt()
    choice match {
      case 1 => ViewPowerPlantData
      case 2 => AnalyzeEnergyGenerationData
      case 3 => GenerateAlerts
      case 4 => Exit
      case _ =>
        println("Invalid choice. Please enter a valid option.")
        getUserChoice
    }
  }

  // Main function to start the application
  def main(args: Array[String]): Unit = {
    val dataFetches = Seq(
      "https://data.fingrid.fi/api/datasets/191/data" -> "hydro.csv",
      "https://data.fingrid.fi/api/datasets/248/data" -> "solar.csv",
      "https://data.fingrid.fi/api/datasets/75/data" -> "wind.csv"
    )

    dataFetches.foreach { case (url, fileName) =>
      fetchEnergyData(url, fileName)
    }

    var continue = true
    while (continue) {
      displayMenu()
      val option = getUserChoice
      if (option == Exit) {
        continue = false
      } else {
        executeOption(option)
      }
    }
  }
}
