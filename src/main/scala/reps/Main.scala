package reps

import akka.actor.ActorSystem
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.language.postfixOps
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

import reps.datacollection.EnergyGenerationDataCollection.fetchEnergyData
import reps.views.PowerPlantView.choice
import reps.dataanalysis.EnergyGenerationDataAnalysis.analyzeData
import reps.control.RenewableControl._
import reps.alerts.AlertGeneration.generateAlerts

//Duc Duong
//Mattias Slotte
//Mengshi Qi

// Define a sealed trait to represent the menu options
sealed trait MenuOption
object MenuOption {
  case object ViewPowerPlantData extends MenuOption
  case object AnalyzeEnergyGenerationData extends MenuOption
  case object GenerateAlerts extends MenuOption
  case object ControlRenewablePlants extends MenuOption
  case object Exit extends MenuOption
}

object Main {
  import MenuOption._

  private val symbols = new DecimalFormatSymbols(Locale.US)
  private val formatter = new DecimalFormat("#.##", symbols)

  // Function to display the menu options
  private def displayMenu(): Unit = {
    println("1. View Power Plant Data")
    println("2. Analyze Energy Generation Data")
    println("3. Generate Alerts")
    println("4. Control Renewable Plants")
    println("5. Exit")
    print("Enter your choice: ")
  }
  // Function to execute the selected menu option
  private def executeOption(option: MenuOption): Unit = option match {
    case ViewPowerPlantData => choice()
    // To use analyzeData, insert the paths to the solar, wind and hydro data in the order (solar, wind, hydro).
    // It will return the statistics as a List[Array[Double]], where each element in the list is an array of doubles,
    // representing the different statistics of each energy generation method in the following order:
    // (mean, median, mode, range, midrange)
    case AnalyzeEnergyGenerationData =>
      val statisticsNames: Array[String] = Array("Mean", "Median", "Mode", "Range", "Midrange")
      val datasetNames: Array[String] = Array("Solar", "Wind", "Hydro")

      println()
      analyzeData("data/solar.csv", "data/wind.csv", "data/hydro.csv").zip(datasetNames).foreach { case (table, datasetName) =>
        println(s"$datasetName Data:")
        val formattedHeader = statisticsNames.map(name => String.format("%-10s", name)).mkString("\t")
        println(formattedHeader)
        val formattedRow = table.map(value => String.format("%-10s", formatter.format(value).toDouble)).mkString("\t")
        println(formattedRow)
        println()
      }
    case GenerateAlerts => generateAlerts()
    case ControlRenewablePlants => runRenewableControlApp()
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
      case 4 => ControlRenewablePlants
      case 5 => Exit
      case _ =>
        println("Invalid choice. Please enter a valid option.")
        getUserChoice
    }
  }


  // Main function to start the application
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("RenewableEnergyManagementSystem")
    val dataFetches = Seq(
      "https://data.fingrid.fi/api/datasets/191/data" -> "hydro.csv",
      "https://data.fingrid.fi/api/datasets/248/data" -> "solar.csv",
      "https://data.fingrid.fi/api/datasets/75/data" -> "wind.csv"
    )
    val fetchFirst = Future {
      dataFetches.foreach { case (url, fileName) =>
        fetchEnergyData(url, fileName)
      }
    }
    Await.result(fetchFirst, Duration.Inf)

    system.scheduler.scheduleAtFixedRate(initialDelay = 15.minutes, interval = 15.minutes) { () =>
      dataFetches.foreach { case (url, fileName) =>
        fetchEnergyData(url, fileName)
      }
    }
    var continue = true
    while (continue) {
      displayMenu()
      val option = getUserChoice
      if (option == Exit) {
        continue = false
        system.terminate()
      } else {
        executeOption(option)
      }
    }
  }
}