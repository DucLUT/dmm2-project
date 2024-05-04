package reps.datacollection

import java.net.{HttpURLConnection, URL}
import java.io.BufferedReader
import java.io.InputStreamReader

// Duc Duong
// Mattias Slotte
// Mengshi Qi

object EnergyGenerationDataCollection {
  def getKey: String = {
    val source = scala.io.Source.fromFile("src/main/scala/reps/.env")
    val key = try source.mkString finally source.close()
    key
  }

  def main(args: Array[String]): Unit = {
    val ApiKey = getKey
    println(ApiKey)

    val apiHydroUrl = "https://data.fingrid.fi/api/datasets/191/data"
    val apiSolarUrl = "https://data.fingrid.fi/api/datasets/248/data"
    val apiWindUrl = "https://data.fingrid.fi/api/datasets/181/data"
    
    val url = new URL(apiHydroUrl)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]

    conn.setRequestMethod("GET")
    conn.setRequestProperty("Cache-Control", "no-cache")
    conn.setRequestProperty("x-api-key",ApiKey)
    val response = conn.getResponseCode
    println(response)
  }
}
