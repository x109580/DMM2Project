package DMM2

import io.circe.generic.auto._
import sttp.client3._
import sttp.client3.circe._

import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.io.StdIn

object EnergyAlertSystem {

  case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
  case class ApiResponse(data: List[TimeSeriesData], pagination: Pagination)
  case class Pagination(total: Int, lastPage: Int, prevPage: Option[Int], nextPage: Option[Int], perPage: Int, currentPage: Int, from: Int, to: Int)

  val apiKey = "6c13b109e4af4a21acde6205f257b426"
  val backend = HttpURLConnectionBackend()
  val historyPeriod = 30

  def main(args: Array[String]): Unit = {
    println("Welcome to the Renewable Energy Monitoring System")
    val currentDate = getInputDate()
    val startDate = currentDate.minusDays(historyPeriod)
    val datasetId = chooseDataset()
    val datasetType = getDatasetType(datasetId)

    fetchDataFromAPI(apiKey, datasetId, startDate, currentDate.minusDays(1)) match {
      case Right(history) =>
        val dynamicThreshold = calculateDynamicThreshold(history.map(_._2))
        println(s"Dynamic threshold set at $dynamicThreshold kWh for $datasetType.")
        monitorCurrentProduction(currentDate, datasetId, datasetType, dynamicThreshold)
      case Left(errorMessage) =>
        println(s"Error fetching historical data: $errorMessage")
    }
  }


  def getInputDate(): LocalDate = {
    println("Please enter the date you want to analyze (DD/MM/YYYY):")
    val input = StdIn.readLine()
    val currentDate = LocalDate.now()

    try {
      val parsedDate = LocalDate.parse(input, DateTimeFormatter.ofPattern("dd/MM/yyyy"))
      if (parsedDate.isAfter(currentDate)) {
        println("The date entered is in the future. Please enter a valid past or present date.")
        getInputDate()
      } else {
        parsedDate
      }
    } catch {
      case e: DateTimeParseException =>
        println("Invalid date format. Please try again.")
        getInputDate()
    }
  }

  def chooseDataset(): Int = {
    println("Select the type of power generation data to fetch (solar, hydro, wind):")
    StdIn.readLine().toLowerCase match {
      case "solar" => 248
      case "hydro" => 191
      case "wind"  => 75
      case _       => println("Invalid choice, defaulting to hydro."); 191
    }
  }

  def getDatasetType(datasetId: Int): String = datasetId match {
    case 248 => "solar"
    case 191 => "hydro"
    case 75  => "wind"
    case _   => "unknown"
  }

  def fetchDataFromAPI(apiKey: String, datasetId: Int, startDate: LocalDate, endDate: LocalDate): Either[String, Seq[(LocalDateTime, Double)]] = {
    val uriString = s"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=${startDate}T00:00:00Z&endTime=${endDate}T23:59:59Z&format=json&pageSize=20000"
    val request = basicRequest.get(uri"$uriString").header("x-api-key", apiKey).response(asJson[ApiResponse])
    val response = request.send(backend)
    response.body match {
      case Right(apiResponse) =>
        Right(apiResponse.data.map(ts =>
          (ZonedDateTime.parse(ts.startTime, DateTimeFormatter.ISO_ZONED_DATE_TIME).toLocalDateTime, ts.value)
        ))
      case Left(error) =>
        Left(s"API request failed: $error")
    }
  }

  def calculateDynamicThreshold(values: Seq[Double]): Double = {
    if (values.nonEmpty) {
      val sortedValues = values.sorted
      val lowerQuartile = sortedValues((sortedValues.length * 0.25).toInt)
      lowerQuartile * 0.75
    } else {
      0.0
    }
  }

  def monitorCurrentProduction(currentDate: LocalDate, datasetId: Int, datasetType: String, dynamicThreshold: Double): Unit = {
    fetchDataFromAPI(apiKey, datasetId, currentDate, currentDate) match {
      case Right(data) =>
        val anomalies = data.filter(_._2 < dynamicThreshold)
        if (anomalies.isEmpty) {
          println("Data normal: No issues detected with today's production.")
        } else {
          anomalies.foreach { case (dateTime, value) =>
            generateAlert(dateTime, value, dynamicThreshold, datasetType)
          }
        }
      case Left(errorMessage) =>
        println(s"Error fetching current data: $errorMessage")
    }
  }

  def generateAlert(dateTime: LocalDateTime, value: Double, threshold: Double, datasetType: String): Unit = {
    println(s"ALERT at ${dateTime.format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))}: Low output detected for $datasetType. Output: $value kWh is below the dynamic threshold of $threshold kWh.")
  }
}
