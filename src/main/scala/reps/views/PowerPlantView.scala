package reps.views

import com.github.tototoshi.csv._

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object PowerPlantView {
  private def sortByTimestamp(data: List[List[String]]): List[List[String]] = {
    val header :: tail = data
    header :: tail.sortBy(row => LocalDateTime.parse(row.head, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")))
  }


  private def sortByValue(data: List[List[String]]): List[List[String]] = {
    val header :: tail = data
    header :: tail.sortBy(row => row.last.toDouble)
  }

  private def displayData(fileName: String, sortBy: Option[String]): Unit = {
    val file = new File(fileName)
    val reader = CSVReader.open(file)
    var data = reader.all()
    reader.close()

    // Apply sorting if requested
    sortBy match {
      case Some("timestamp") => data = sortByTimestamp(data)
      case Some("value") => data = sortByValue(data)
      case _ => // No sorting
    }

    // Display headers
    val headers = data.head.mkString("\t")
    println(headers)

    // Display rows
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
    data.tail.foreach { row =>
      val formattedRow = row.updated(0, LocalDateTime.parse(row.head, dateFormatter).format(DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")))
        .updated(1, LocalDateTime.parse(row(1), dateFormatter).format(DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")))
      println(formattedRow.mkString("\t"))
    }
  }

  def displayPowerPlantData(): Unit = {
    println("Power Plant Data")
  }

  def choice(): Unit = {
    println("What option do you want to view?")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    print("Enter your choice: ")

    scala.io.StdIn.readInt() match {
      case 1 =>
        println("Sorting options:")
        println("1. Sort by timestamp")
        println("2. Sort by value")
        print("Enter your sorting choice: ")
        val sortBy = scala.io.StdIn.readInt() match {
          case 1 => Some("timestamp")
          case 2 => Some("value")
          case _ => None
        }
        displayData("data/solar.csv", sortBy)
      case 2 =>
        println("Sorting options:")
        println("1. Sort by timestamp")
        println("2. Sort by value")
        print("Enter your sorting choice: ")
        val sortBy = scala.io.StdIn.readInt() match {
          case 1 => Some("timestamp")
          case 2 => Some("value")
          case _ => None
        }
        displayData("data/wind.csv", sortBy)
      case 3 =>
        println("Sorting options:")
        println("1. Sort by timestamp")
        println("2. Sort by value")
        print("Enter your sorting choice: ")
        val sortBy = scala.io.StdIn.readInt() match {
          case 1 => Some("timestamp")
          case 2 => Some("value")
          case _ => None
        }
        displayData("data/hydro.csv", sortBy)
      case _ => println("Invalid choice")
    }
  }
}
