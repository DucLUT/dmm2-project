import java.net.{HttpURLConnection, URL}
import java.io.{BufferedReader, FileWriter, InputStreamReader}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.json4s._
import org.json4s.native.JsonMethods._

object Main {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def getKey: String = {
    val source = scala.io.Source.fromFile("src/main/scala/reps/.env")
    val key = try source.mkString finally source.close()
    key
  }

  def main(args: Array[String]): Unit = {
    val ApiKey = getKey
    val date = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)
    val apiUrl = s"https://data.fingrid.fi/api/datasets/75/data"
    val url = new URL(apiUrl)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]

    conn.setRequestMethod("GET")
    conn.setRequestProperty("Cache-Control", "no-cache")
    conn.setRequestProperty("Content-Type", "application/json")
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
      val csvFile = new FileWriter("data.csv")
      csvFile.write("startTime,endTime,value\n")
      data.foreach { item =>
        val startTime = (item \ "startTime").extract[String]
        val endTime = (item \ "endTime").extract[String]
        val value = (item \ "value").extract[Double]
        csvFile.write(s"$startTime,$endTime,$value\n")
      }
      csvFile.close()
      println("CSV file created successfully.")
    } else {
      println(s"Failed to retrieve data. Response code: $responseCode")
    }
  }
}
