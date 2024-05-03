package example
import java.net.{HttpURLConnection, URL}
import java.io.BufferedReader
import java.io.InputStreamReader

object Main {
  def main(args: Array[String]): Unit = {
    val API_KEY = "yourapikey"
    val api_URL = "https://data.fingrid.fi/api/notifications/active"
    val url = new URL(api_URL)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]

    conn.setRequestMethod("GET")
    conn.setRequestProperty("Cache-Control", "no-cache")
    conn.setRequestProperty("x-api-key",API_KEY)
    val response = conn.getResponseCode
    println(response)
  }

}