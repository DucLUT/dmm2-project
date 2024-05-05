package reps.datacollection
import scala.util.{Try, Success, Failure}
import java.io.File
import java.net.{HttpURLConnection, URL}
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileWriter
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.language.postfixOps

// Duc Duong
// Mattias Slotte
// Mengshi Qi

object EnergyGenerationDataCollection {
  implicit val formats: DefaultFormats.type = DefaultFormats
  // private var csvCreated = false
  private def getKey: String = Try {
    val source = scala.io.Source.fromFile("src/main/scala/reps/.env")
    try source.mkString finally source.close()
  } getOrElse {
    println("Error: Unable to read API key from .env file.")
    ""
  }


  def fetchEnergyData(apiUrl: String, fileName: String): Unit = {
    val ApiKey = getKey
    if (ApiKey.nonEmpty) {
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

      processResponse(response.toString(), fileName)
    } else {
      println(s"Failed to retrieve data. Response code: $responseCode")
    }
    }


def processResponse(response: String, fileName: String): Unit = try{
      val json = parse(response.toString)
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
          csvFile.write(s"$dataLine\n") // Append new data to the file
        }
      }

      csvFile.close()

  if (!fileExists) {
    println("CSV file created successfully.")
  } else {
    println("New data appended to the CSV file.")
  }
} catch {
  case e: Exception =>
    println(s"Error processing response or writing to file: ${e.getMessage}")
}
  }


}
