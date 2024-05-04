package DMM2

import io.circe.generic.auto._
import sttp.client3._
import sttp.client3.circe._

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.temporal.WeekFields
import java.util.Locale
import scala.io.StdIn

object EnergyDataProcessor {

  def main(args: Array[String]): Unit = {
    val startDate = getInputDate("start")
    val endDate = getInputDate("end")
    val datasetId = chooseDataset()

    try {
      val apiKey = "6c13b109e4af4a21acde6205f257b426"
      val dataFromAPI = fetchDataFromAPI(apiKey, datasetId, startDate, endDate)
      if (dataFromAPI.isEmpty) {
        println("No available data from API for the selected date range. Please choose another range.")
      } else {
        println("\nData from API:")
        displayProductionData(dataFromAPI)
        println("\nDetailed Analysis:")
        displayDataAnalysis(dataFromAPI)
      }
    } catch {
      case e: Exception =>
        println(s"Error processing data: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def chooseDataset(): Int = {
    println("Select the type of power generation data to fetch: (solar, hydro, wind)")
    val choice = StdIn.readLine().toLowerCase()
    choice match {
      case "solar" => 248
      case "hydro" => 191
      case "wind" => 75
      case _ =>
        println("Invalid choice, defaulting to hydro power.")
        191
    }
  }

  def fetchDataFromAPI(apiKey: String, datasetId: Int, startDate: LocalDate, endDate: LocalDate): Seq[(LocalDate, Int, Double)] = {
    val request = basicRequest
      .get(uri"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=${startDate}T00:00:00Z&endTime=${endDate}T23:59:59Z&format=json&pageSize=20000")
      .header("x-api-key", apiKey)
      .response(asJson[ApiResponse])

    implicit val backend = HttpURLConnectionBackend()

    val response = request.send()

    response.body match {
      case Right(apiResponse) =>
        apiResponse.data.map { ts =>
          val date = LocalDate.parse(ts.startTime.substring(0, 10))
          val hour = ts.startTime.substring(11, 13).toInt
          (date, hour, ts.value)
        }
      case Left(error) =>
        println(s"Error fetching data from API: $error")
        Seq.empty
    }
  }

  def getInputDate(dateType: String): LocalDate = {
    var date: LocalDate = null
    while (date == null) {
      println(s"Please enter the $dateType date (DD/MM/YYYY):")
      val dateStr = StdIn.readLine()
      try {
        date = LocalDate.parse(dateStr, DateTimeFormatter.ofPattern("dd/MM/yyyy"))
      } catch {
        case e: DateTimeParseException =>
          println(s"Invalid date format. Please enter the date in the format 'DD/MM/YYYY'.")
      }
    }
    date
  }



  def displayProductionData(data: Seq[(LocalDate, Int, Double)]): Unit = {
    // Combine production data with the same date and hour
    val combinedData = data.groupBy { case (date, hour, _) => (date, hour) }
      .mapValues(_.map(_._3).sum)

    val sortedData = combinedData.toList.sortBy { case ((date, hour), _) => (date, hour) }

    val dailyTotals = calculateDailyTotals(data)
    val sortedDailyTotals = dailyTotals.toSeq.sortBy(_._1)

    val weeklyTotals = calculateWeeklyTotals(dailyTotals)
    val sortedWeeklyTotals = weeklyTotals.toSeq.sortBy(_._1)

    val monthlyTotals = calculateMonthlyTotals(dailyTotals)
    val sortedMonthlyTotals = monthlyTotals.toSeq.sortBy(_._1)

    println("Hourly Production:")
    sortedData.foreach { case ((date, hour), production) =>
      println(s"$date $hour:00: $production kWh")
    }

    println("\nDaily Totals:")
    sortedDailyTotals.foreach { case (date, total) =>
      println(s"$date: $total kWh")
    }

    println("\nWeekly Totals:")
    sortedWeeklyTotals.foreach { case (week, total) =>
      println(s"Week $week: $total kWh")
    }

    println("\nMonthly Totals:")
    sortedMonthlyTotals.foreach { case (month, total) =>
      println(s"$month: $total kWh")
    }
  }

  def displayDataAnalysis(data: Seq[(LocalDate, Int, Double)]): Unit = {
    val hourlyData = data.map(_._3) // Collect all hourly data points together for overall statistics
    val dailyData = calculateDailyTotals(data).values.toSeq
    val weeklyData = calculateWeeklyTotals(calculateDailyTotals(data)).values.toSeq
    val monthlyData = calculateMonthlyTotals(calculateDailyTotals(data)).values.toSeq

    println("Overall Hourly Statistics:")
    displayStatistics(hourlyData)

    println("\nDaily Statistics:")
    displayStatistics(dailyData)

    println("\nWeekly Statistics:")
    displayStatistics(weeklyData)

    println("\nMonthly Statistics:")
    displayStatistics(monthlyData)
  }

  def displayStatistics(values: Seq[Double]): Unit = {
    if (values.isEmpty) {
      println("No data to analyze.")
    } else {
      val mean = values.sum / values.length
      val sortedValues = values.sorted
      val median = if (values.length % 2 == 1) sortedValues(values.length / 2)
      else (sortedValues(values.length / 2 - 1) + sortedValues(values.length / 2)) / 2
      val mode = values.groupBy(identity).maxBy(_._2.size)._1
      val range = sortedValues.last - sortedValues.head
      val midrange = (sortedValues.head + sortedValues.last) / 2

      println(s"Mean: $mean")
      println(s"Median: $median")
      println(s"Mode: $mode")
      println(s"Range: $range")
      println(s"Midrange: $midrange")
    }
  }

  def calculateDailyTotals(data: Seq[(LocalDate, Int, Double)]): Map[LocalDate, Double] = {
    data.groupBy(_._1).view.mapValues(_.map(_._3).sum).toMap
  }

  def calculateWeeklyTotals(dailyTotals: Map[LocalDate, Double]): Map[Int, Double] = {
    dailyTotals.groupBy {
      case (date, _) =>
        val weekFields = WeekFields.of(Locale.getDefault).weekOfWeekBasedYear()
        date.get(weekFields)
    }.mapValues(_.map(_._2).sum).toMap
  }

  def calculateMonthlyTotals(dailyTotals: Map[LocalDate, Double]): Map[String, Double] = {
    dailyTotals.groupBy {
      case (date, _) =>
        date.getMonth.toString + " " + date.getYear
    }.mapValues(_.map(_._2).sum).toMap
  }

  case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
  case class ApiResponse(data: List[TimeSeriesData], pagination: Pagination)
  case class Pagination(total: Int, lastPage: Int, prevPage: Option[Int], nextPage: Option[Int], perPage: Int, currentPage: Int, from: Int, to: Int)
}
