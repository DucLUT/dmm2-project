package reps.views

import com.github.tototoshi.csv._
import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object PowerPlantView {
  def displayPowerPlantData(): Unit = {
    println("Power Plant Data")
  }

  def displayView(fileName: String): Unit = {
    val file = new File(fileName)
    val reader = CSVReader.open(file)
    val data = reader.all()
    reader.close()

    // Display headers
    val headers = data.head.mkString("\t")
    println(headers)

    // Display rows
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
    data.tail.foreach { row =>
      val formattedRow = row.updated(0, LocalDateTime.parse(row(0), dateFormatter).format(DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")))
        .updated(1, LocalDateTime.parse(row(1), dateFormatter).format(DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")))
      println(formattedRow.mkString("\t"))
    }
  }

  def choice(): Unit = {
    println("What option do you want to view?")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    print("Enter your choice: ")

    scala.io.StdIn.readInt() match {
      case 1 => displayView("data/solar.csv")
      case 2 => displayView("data/wind.csv")
      case 3 => displayView("data/hydro.csv")
      case _ => println("Invalid choice")
    }
  }
}

