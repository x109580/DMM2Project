package DMM2

import java.io.{File, PrintWriter, FileWriter}
import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.io.StdIn
import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._

object monitor_energy {
  def main(args: Array[String]): Unit = {
    val apiKey = "6c13b109e4af4a21acde6205f257b426" // Use your actual API key
    val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    val fileDateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

    def fetchData(datasetId: Int, date: LocalDate): List[TimeSeriesData] = {
      val formattedDate = date.format(fileDateFormatter)
      val startTime = s"${formattedDate}T00:00:00Z"
      val endTime = s"${formattedDate}T23:59:59Z"
      val request = basicRequest
        .get(uri"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=$startTime&endTime=$endTime&format=json&pageSize=20000")
        .header("x-api-key", apiKey)
        .response(asJson[ApiResponse])

      implicit val backend = HttpURLConnectionBackend()
      val response = request.send()
      response.body match {
        case Right(apiResponse) if apiResponse.data.nonEmpty => apiResponse.data
        case Right(_) =>
          println("No data available for the provided dates. Please enter a different date range.")
          List()
        case Left(error) =>
          println(s"Error fetching data: $error")
          List()
      }
    }

    def displayData(data: List[TimeSeriesData]): Unit = {
      data.foreach(ts => println(s"Time from ${ts.startTime} to ${ts.endTime}, Value: ${ts.value} MW"))
    }

    def storeData(data: Double, resourceType: String, date: String): Unit = {
      val outputPath = "all_energy_data.txt"
      val writer = new PrintWriter(new FileWriter(new File(outputPath), true))
      try {
        writer.println(s"$date - $resourceType production: $data MW")
      } finally {
        writer.close()
      }
      println(s"Data for $resourceType on $date appended in $outputPath")
    }

    def getInputDate(): LocalDate = {
      println("Enter the date (DD/MM/YYYY):")
      val input = StdIn.readLine()
      try {
        val date = LocalDate.parse(input, dateFormatter)
        if (date.isAfter(LocalDate.now())) {
          println("Future dates are not allowed. Please enter a past or current date.")
          getInputDate()
        } else {
          date
        }
      } catch {
        case e: DateTimeParseException =>
          println("Invalid date format. Please use DD/MM/YYYY.")
          getInputDate() // Recursive call on error
      }
    }

    def readDoubleWithPrompt(prompt: String): Double = {
      println(prompt)
      val input = StdIn.readLine()
      try {
        input.toDouble // Return valid double
      } catch {
        case _: NumberFormatException =>
          println("Invalid input. Please enter a valid number.")
          readDoubleWithPrompt(prompt) // Recursive call on error
      }
    }


    def loop(): Unit = {
      println("Select an option:")
      println("1: Monitor solar energy data")
      println("2: Monitor wind energy data")
      println("3: Monitor hydropower data")
      println("0: Exit")
      val choice = StdIn.readLine()

      choice match {
        case "1" | "2" | "3" =>
          val datasetId = choice match {
            case "1" => 248
            case "2" => 75
            case "3" => 191
          }
          val date = getInputDate()
          val data = fetchData(datasetId, date)
          val totalProduction = data.map(_.value).sum
          println(s"Total production for the day: $totalProduction MW")

          val factor = readDoubleWithPrompt("Enter adjustment factor (e.g., 1.05 for a 5% increase):")
          val adjustedData = totalProduction * factor
          println(s"Adjusted data: $adjustedData MW")
          storeData(adjustedData, choice match {
            case "1" => "solar"
            case "2" => "wind"
            case "3" => "hydropower"
          }, date.format(fileDateFormatter))
          loop() // Recursive call to continue

        case "0" =>
          println("Exiting...")

        case _ =>
          println("Invalid option, please select again.")
          loop() // Recursive call to handle invalid options
      }
    }

    loop()

    case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
    case class ApiResponse(data: List[TimeSeriesData], pagination: Pagination)
    case class Pagination(total: Int, lastPage: Int, prevPage: Option[Int], nextPage: Option[Int], perPage: Int, currentPage: Int, from: Int, to: Int)
  }
}


