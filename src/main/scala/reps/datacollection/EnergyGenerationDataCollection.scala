package reps.datacollection
import java.io.File
import java.net.{HttpURLConnection, URL}
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileWriter
import org.json4s._
import org.json4s.native.JsonMethods._
import akka.actor.ActorSystem
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.io.Source
// Duc Duong
// Mattias Slotte
// Mengshi Qi

object EnergyGenerationDataCollection {
  implicit val formats: DefaultFormats.type = DefaultFormats
  private var csvCreated = false
  private def getKey: String = {
    val source = scala.io.Source.fromFile("src/main/scala/reps/.env")
    val key = try source.mkString finally source.close()
    key
  }

  def fetchEnergyData(apiUrl: String, fileName: String): Unit = {
    val ApiKey = getKey
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
      val existingData = if (fileExists) Source.fromFile(filePath).getLines().toSet else Set.empty[String]

      // Append new data only if it's not already present in the file
      data.foreach { item =>
        val dataLine = s"${(item \ "startTime").extract[String]},${(item \ "endTime").extract[String]},${(item \ "value").extract[Double]}"
        if (!existingData.contains(dataLine)) {
          csvFile.write(s"$dataLine\n") // Append new data to the file
        }
      }

      csvFile.close()

      if (fileExists) {
//        println("New data appended to the CSV file.")
      } else {
        println("CSV file created successfully.")
      }
    } else {
      println(s"Failed to retrieve data. Response code: $responseCode")
    }
  }


}
