package reps.views
import scala.util.Try
import com.github.tototoshi.csv._

import java.io.File
import java.time.{LocalDateTime, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.temporal.ChronoUnit
import java.time.LocalDate
import scala.util.Success
import scala.util.Failure

object PowerPlantView {
  //The filterLast24Hours function takes a list of lists of strings and a DateTimeFormatter as input.
  // It filters out any rows that do not have exactly three columns of data and attempts
  // to parse the timestamps in the first two columns using the provided DateTimeFormatter.
  private def sortByTimestamp(data: List[List[String]]): Option[List[List[String]]] = Try {
    val header :: tail = data
    header :: tail.sortBy(row => LocalDateTime.parse(row.head, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")))
  }.toOption
  //The sortByValue function takes a list of lists of strings as input
  // and sorts the data by the last column, which is assumed to be a numeric value.
  private def sortByValue(data: List[List[String]]): Option[List[List[String]]] = Try {
    val header :: tail = data
    header :: tail.sortBy(row => row.last.toDouble)
  }.toOption

  // Filter data based on time period
  private def filterByTimePeriod(data: List[List[String]], amount: Long, period: ChronoUnit): Option[List[List[String]]] = {
    Try {
      val now = LocalDateTime.now(ZoneId.of("UTC"))
      val startOfPeriod = now.minus(amount, period)
      val header :: tail = data
      val filteredData = header :: tail.filter { row =>
        if (row.headOption.contains("startTime")) {
          // Skip header row
          false
        } else {
          Try {
            val timestamp = LocalDateTime.parse(row.head, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX"))
            // Check if the timestamp is after the specified period from the current time
            timestamp.isAfter(startOfPeriod) && timestamp.isBefore(now)
          }.getOrElse {
            // Print warning message for parsing error
            println(s"Warning: Failed to parse timestamp in row: $row")
            false // Skip the row with invalid timestamp
          }
        }
      }
      println("Filtered Results:")
      filteredData.foreach(row => println(row.mkString("\t")))
      Some(filteredData)
    } match {
      case Success(value) => value
      case Failure(_) => None
    }
  }

  // Allows the user to search for data by date, if the date is in the correct format (dd/MM/yyyy).
  private def searchByDate(data: List[List[String]], dateString: String): Option[List[List[String]]] = {
    Try {
      val date = LocalDate.parse(dateString, DateTimeFormatter.ofPattern("dd/MM/yyyy"))
      val header :: tail = data
      val searchData = header :: tail.filter { row =>
        Try {
          val timestamp = LocalDateTime.parse(row.head, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX"))
          timestamp.toLocalDate.isEqual(date)
        } match {
          case Success(value) => value
          case Failure(e) =>
            println(s"Failed to parse date: ${row.head}")
            false
        }
      }
      println("Search Results:")
      searchData.foreach(row => println(row.mkString("\t")))
      Some(searchData)
    } match {
      case Success(value) => value
      case Failure(e) =>
        println(s"Failed to parse input date: $dateString")
        None
    }
  }

  //the formatTimestamps function takes a list of lists of strings and a DateTimeFormatter as input.
  // It formats the timestamps in the data using the provided DateTimeFormatter and returns the formatted data.
  // The function first extracts the header row from the data and updates the first two columns with empty strings.
  // It then filters out any rows that do not have exactly three columns of data.
  // For each valid row, the function attempts to parse the timestamps in the first two columns using the provided DateTimeFormatter.
  private def formatTimestamps(data: List[List[String]], dateFormatter: DateTimeFormatter): Option[List[List[String]]] = Try {
    val header :: tail = data
    val formattedHeader = header.updated(0, "").updated(1, "")
    val validData = tail.filter(_.size == 3)
    val formattedData = validData.map { row =>
      try {
        row.updated(0, LocalDateTime.parse(row.head, dateFormatter).format(DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")))
          .updated(1, LocalDateTime.parse(row(1), dateFormatter).format(DateTimeFormatter.ofPattern("yyyy-MM-dd h:mm a")))
      } catch {
        // If parsing fails, return the row as is
        case _: Throwable => row
      }
    }
    formattedHeader :: formattedData
  }.toOption



  //The displayData function reads the data from the given file and displays it in a tabular format.
  // The sortBy parameter is an optional parameter that specifies the column to sort the data by.
  // If sortBy is "timestamp", the data is sorted by timestamp, and if sortBy is "value", the data is sorted by value.
  // If sortBy is not provided, the data is displayed as is.
  private def displayData(fileName: String, sortBy: Option[String]): Unit = {
    Try {
      val file = new File(fileName)
      val reader = CSVReader.open(file)
      val data = reader.all()
      reader.close()
      data
    }.toOption.foreach { data =>
      val sortedData = sortBy match {
        case Some("timestamp") => sortByTimestamp(data)
        case Some("value") => sortByValue(data)
        case Some("search") =>
          print("Enter the date to search (dd/MM/yyyy): ")
          val dateString = scala.io.StdIn.readLine()
          searchByDate(data, dateString) match {
            case Some(value) => Some(value)
            case None =>
              println("Error: Date must be in the format dd/MM/yyyy")
              None
          }
        case Some("filter") =>
          println("Filter options:")
          println("1. Last 24 hours")
          println("2. Last 7 days")
          println("3. Last 30 days")
          print("Enter your filter choice: ")
          val period = scala.io.StdIn.readInt() match {
            case 1 => Some((24, ChronoUnit.HOURS))
            case 2 => Some((7, ChronoUnit.DAYS))
            case 3 => Some((1, ChronoUnit.MONTHS))
            case _ => None
          }
          period.flatMap(p => filterByTimePeriod(data, p._1, p._2)) match {
            case Some(value) => Some(value)
            case None =>
              println("Error: Invalid time period")
              None
          }
        case _ => Some(data)
      }

      sortedData.foreach { sorted =>
        // Display headers
        val headers = sorted.head.mkString("\t")
        println(headers)
        // Display rows with formatted timestamps
        val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
        formatTimestamps(sorted.tail, dateFormatter).foreach { formatted =>
          formatted.foreach(row => println(row.mkString("\t")))
        }
      }
    }
  }


  private def displayPlantData(plantName: String, fileName: String, sortBy: Option[String]): Unit = {
    println(s"$plantName Data:")
    displayData(fileName, sortBy)
  }

  private def printSortingOptions(): Unit = {
    println("Sorting options:")
    println("1. Sort by timestamp")
    println("2. Sort by value")
    println("3. Search by date")
    println("4. Filter by time period")
    print("Enter your sorting choice: ")
  }


  //The choice function displays the options for viewing the data and reads the user's choice.
  // Based on the user's choice, it displays the data for solar, wind, hydro, or all plants.
  // It also provides an option to view the last 24 hours of data for all plants.
  def choice(): Unit = {
    println("Which data do you want to view?")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    println("4. All")
    println("5. Last 24 hours data for all plants")
    print("Enter your choice: ")

    scala.io.StdIn.readInt() match {
      case 1 =>
        printSortingOptions()
        val sortBy = scala.io.StdIn.readInt() match {
          case 1 => Some("timestamp")
          case 2 => Some("value")
          case 3 => Some("search")
          case 4 => Some("filter")
          case _ => None
        }
        displayPlantData("Solar", "data/solar.csv", sortBy)
      case 2 =>
        printSortingOptions()
        val sortBy = scala.io.StdIn.readInt() match {
          case 1 => Some("timestamp")
          case 2 => Some("value")
          case 3 => Some("search")
          case 4 => Some("filter")
          case _ => None
        }
        displayPlantData("Wind", "data/wind.csv", sortBy)
      case 3 =>
        printSortingOptions()
        val sortBy = scala.io.StdIn.readInt() match {
          case 1 => Some("timestamp")
          case 2 => Some("value")
          case 3 => Some("search")
          case 4 => Some("filter")
          case _ => None
        }
        displayPlantData("Hydro", "data/hydro.csv", sortBy)
      case 4 =>
        displayPlantData("Solar", "data/solar.csv", Some("timestamp"))
        displayPlantData("Wind", "data/wind.csv", Some("timestamp"))
        displayPlantData("Hydro", "data/hydro.csv", Some("timestamp"))
      case 5 =>
        println("Last 24 hours data for all plants:")
        val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX")
        val solarFileName = "data/solar.csv"
        val windFileName = "data/wind.csv"
        val hydroFileName = "data/hydro.csv"
        //        val filteredSolarData = formatTimestamps(filterLast24Hours(CSVReader.open(new File(solarFileName)).all(), dateFormatter), dateFormatter)
        //        val filteredWindData = formatTimestamps(filterLast24Hours(CSVReader.open(new File(windFileName)).all(), dateFormatter), dateFormatter)
        //        val filteredHydroData = formatTimestamps(filterLast24Hours(CSVReader.open(new File(hydroFileName)).all(), dateFormatter), dateFormatter)
        displayPlantData("Solar", solarFileName, Some("timestamp"))
        displayPlantData("Wind", windFileName, Some("timestamp"))
        displayPlantData("Hydro", hydroFileName, Some("timestamp"))

      case _ => println("Invalid choice")
    }
  }
}