package DMM2

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.io.StdIn
import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import java.io.{File, PrintWriter}


object collect extends App {
  val apiKey = "6c13b109e4af4a21acde6205f257b426"
  val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def fetchData(datasetId: Int, startDate: String, endDate: String): List[TimeSeriesData] = {
    val startTime = s"${LocalDate.parse(startDate, dateFormatter)}T00:00:00Z"
    val endTime = s"${LocalDate.parse(endDate, dateFormatter)}T23:59:59Z"
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

  def getValidDate(prompt: String): String = {
    println(prompt)
    val date = StdIn.readLine()
    try {
      val parsedDate = LocalDate.parse(date, dateFormatter)
      if (parsedDate.isAfter(LocalDate.now())) {
        println("Future dates are not allowed as there's no data available. Please enter a past or current date.")
        getValidDate(prompt)
      } else {
        date
      }
    } catch {
      case e: DateTimeParseException =>
        println("Invalid date format. Please use DD/MM/YYYY.")
        getValidDate(prompt)
    }
  }

  def mainLoop(): Unit = {
    println("Select an option:")
    println("1: Collect solar energy data")
    println("2: Collect wind energy data")
    println("3: Collect hydropower data")
    println("0: Exit")
    val choice = StdIn.readLine()

    choice match {
      case "1" | "2" | "3" =>
        val (datasetId, resourceType) = choice match {
          case "1" => (248, "solar")
          case "2" => (75, "wind")
          case "3" => (191, "hydropower")
        }
        val startDate = getValidDate("Enter the start date (DD/MM/YYYY):")
        val endDate = getValidDate("Enter the end date (DD/MM/YYYY):")
        if (LocalDate.parse(startDate, dateFormatter).isBefore(LocalDate.now().plusDays(1))) {
          val data = fetchData(datasetId, startDate, endDate)
          if (data.nonEmpty) {
            displayData(data)
            storeData(data, s"$resourceType-power-$startDate-to-$endDate.txt", resourceType, startDate, endDate)
          }
        }
        mainLoop()  // Recursive call to continue

      case "0" =>
        println("Exiting...")

      case _ =>
        println("Invalid option, please select again.")
        mainLoop()  // Recursive call on invalid option
    }
  }

  def displayData(data: List[TimeSeriesData]): Unit = {
    data.foreach { ts =>
      println(s"Time from ${ts.startTime} to ${ts.endTime}, Value: ${ts.value} MW")
    }
  }

  def storeData(data: List[TimeSeriesData], outputPath: String, resourceType: String, startDate: String, endDate: String): Unit = {
    if (data.isEmpty) {
      println("No data to store.")
      return
    }


    val dateFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val formattedStartDate = LocalDate.parse(startDate, DateTimeFormatter.ofPattern("dd/MM/yyyy")).format(dateFormatter)
    val formattedEndDate = LocalDate.parse(endDate, DateTimeFormatter.ofPattern("dd/MM/yyyy")).format(dateFormatter)
    val filename = s"${resourceType}-power-$formattedStartDate-to-$formattedEndDate.txt"
    val fullOutputPath = outputPath + File.separator + filename


    val file = new File(fullOutputPath)
    file.getParentFile.mkdirs()

    val writer = new PrintWriter(file)
    try {
      data.foreach { ts =>
        writer.println(s"Data for period from ${ts.startTime} to ${ts.endTime}: ${ts.value} MW")
      }
    } catch {
      case e: Exception => println(s"Failed to write to file: $e")
    } finally {
      writer.close()
    }
    println(s"Data stored in $fullOutputPath")
  }
  mainLoop()

  case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
  case class ApiResponse(data: List[TimeSeriesData], pagination: Pagination)
  case class Pagination(total: Int, lastPage: Int, prevPage: Option[Int], nextPage: Option[Int], perPage: Int, currentPage: Int, from: Int, to: Int)
}


