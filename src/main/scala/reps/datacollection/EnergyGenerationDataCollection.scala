package reps.datacollection

import java.io.File
import java.net.{HttpURLConnection, URL}
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileWriter
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.language.postfixOps
import scala.io.Source

// Duc Duong
// Mattias Slotte
// Mengshi Qi

object EnergyGenerationDataCollection {
  implicit val formats: DefaultFormats.type = DefaultFormats
  // private var csvCreated = false
  // Retrieves the API key from the .env file located within the project.
  private def getKey: Option[String] = {
    try {
      val source = Source.fromFile("src/main/scala/reps/.env")
      try Some(source.mkString.trim) finally source.close()
    } catch {
      case _: Exception =>
        println("Error: Unable to read API key from .env file.")
        None
    }
  }

  // High-level function that orchestrates the fetching and processing of data.
  def fetchEnergyData(apiUrl: String, fileName: String): Option[Unit] = {
    for {
      apiKey <- getKey
      response <- fetchDataFromApi(apiUrl, apiKey)
      _ <- processResponse(response, fileName)
    } yield ()
  }


  // Performs an HTTP GET request to fetch data from the provided API URL using the API key.
  private def fetchDataFromApi(apiUrl: String, ApiKey: String): Option[String] = {
  try {
    val url = new URL(apiUrl)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]

    conn.setRequestMethod("GET")
    conn.setRequestProperty("Cache-Control", "no-cache")
    conn.setRequestProperty("x-api-key", ApiKey)

    val responseCode = conn.getResponseCode
    if (responseCode == HttpURLConnection.HTTP_OK) {
      val in = new BufferedReader(new InputStreamReader(conn.getInputStream))
      val response = new StringBuilder
      var inputLine = in.readLine()
      while (inputLine != null) {
        response.append(inputLine)
        inputLine = in.readLine()
      }
      in.close()
      Some(response.toString)

    } else {
      println(s"Failed to retrieve data. Response code: $responseCode")
      None
    }
  } catch {
    case _: Exception =>
      println("Error during API request.")
      None
  }
}

  // Processes the JSON response from the API and writes it to a CSV file if it does not already contain the data.
  private def processResponse(response: String, fileName: String): Option[Unit] = {
      try {
        val json = parse(response)
        val data = (json \ "data").extract[List[JValue]]
        val filePath = s"data/$fileName"

        val file = new File(filePath)
        val fileExists = file.exists()

        val csvFile = new FileWriter(filePath, true) // Append mode

        if (!fileExists) {
          csvFile.write("startTime,endTime,value\n") // Write header if the file is newly created
        }

        // Read existing data from the file
        val existingData = if (fileExists) {
          val source = scala.io.Source.fromFile(filePath)
          try {
            source.getLines().toSet
          } finally {
            source.close()
          }
        } else Set.empty[String]

        // Append new data only if it's not already present in the file
        data.foreach { item =>
          val dataLine = s"${(item \ "startTime").extract[String]},${(item \ "endTime").extract[String]},${(item \ "value").extract[Double]}"
          if (!existingData.contains(dataLine)) {
            csvFile.write(s"$dataLine\n")
          }
        }

        csvFile.close()

        if (!fileExists) println("CSV file created successfully.")
        Some(())
      } catch {
        case _: Exception =>
          println("Error processing response or writing to file.")
          None
      }
    }
    }
