package DMM2

import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import java.io.{File, PrintWriter}
import scala.io.{Source, StdIn}
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation
import javax.swing._
import java.awt.BorderLayout
import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.util.{Using, Try}

object EnergyManagementSystem extends App {
  val DatasetId = 74
  val Format = "json"
  val PageSize = 100
  val ApiKey = "6c13b109e4af4a21acde6205f257b426"
  val DateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def fetchData(date: String): Option[List[TimeSeriesData]] = {
    Try(LocalDate.parse(date, DateFormatter)).toOption.flatMap { localDate =>
      val formattedDate = localDate.format(DateTimeFormatter.ISO_LOCAL_DATE)
      val startTime = s"${formattedDate}T00:00:00Z"
      val endTime = s"${formattedDate}T23:59:59Z"
      val request = basicRequest
        .get(uri"https://data.fingrid.fi/api/datasets/$DatasetId/data?startTime=$startTime&endTime=$endTime&format=$Format&pageSize=$PageSize")
        .header("x-api-key", ApiKey)
        .response(asJson[ApiResponse])

      implicit val backend = HttpURLConnectionBackend()
      val response = request.send()

      response.body.toOption.map { apiResponse =>
        Using(new PrintWriter(new File("energy_data.txt"))) { writer =>
          apiResponse.data.foreach(ts => writer.println(s"${ts.startTime},${ts.value}"))
          apiResponse.data
        }.getOrElse(Nil)
      }
    }
  }

  def displayChart(filename: String, title: String): Unit = {
    val file = new File(filename)
    if (!file.exists()) {
      println(s"File not found: $filename. Please check the path and try again.")
      return
    }

    val dataset = new DefaultCategoryDataset()
    Using(Source.fromFile(file)) { source =>
      source.getLines().foreach { line =>
        try {
          val Array(time, value) = line.split(",").map(_.trim)
          dataset.addValue(value.toDouble, "Energy", time)
        } catch {
          case _: Exception => println("Skipping invalid line.")
        }
      }
    }.getOrElse(println("Failed to read the file"))

    val chart = ChartFactory.createLineChart(title, "Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false)
    val panel = new ChartPanel(chart)
    val frame = new JFrame(title)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.add(panel)
    frame.pack()
    frame.setVisible(true)
  }

  def mainMenu(): Unit = {
    println("Select an option:")
    println("1: View Energy Generation Data")
    println("2: View Energy Storage Data from file")
    println("3: Adjust Data")
    println("0: Exit")
    val choice = StdIn.readLine()

    choice match {
      case "1" | "3" =>
        println("Enter the date (DD/MM/YYYY):")
        val date = StdIn.readLine()
        fetchData(date)
        if (choice == "1") {
          displayChart("energy_data.txt", "Original Energy Data")
        } else {
          val factor = readPositiveDouble()
          adjustData(factor, "energy_data.txt", "adjusted_data.txt")
          displayChart("adjusted_data.txt", "Adjusted Energy Data")
        }
        mainMenu()
      case "2" =>
        val filePath = readValidFilePath()
        displayChart(filePath, "Storage Power")
        mainMenu()  // Recursive call
      case "0" =>
      case _ =>
        println("Invalid option. Please try again.")
        mainMenu()  // Recursive call for invalid input
    }
  }

  def readPositiveDouble(): Double = {
    println("Enter adjustment factor (e.g., 1.05 for a 5% increase):")
    Try(StdIn.readDouble()).toOption match {
      case Some(factor) if factor > 0 => factor
      case _ =>
        println("Factor must be greater than 0 and a valid number. Please enter a valid factor.")
        readPositiveDouble()
    }
  }

  def readValidFilePath(): String = {
    println("Enter the file path for storage data:")
    val filePath = StdIn.readLine()
    if (new File(filePath).exists()) filePath
    else {
      println("File not found. Please enter a correct file path.")
      readValidFilePath()
    }
  }

  def adjustData(factor: Double, inputFilename: String, outputFilename: String): Unit = {
    Using(Source.fromFile(inputFilename)) { source =>
      Using(new PrintWriter(new File(outputFilename))) { writer =>
        source.getLines().foreach { line =>
          val Array(time, value) = line.split(",")
          val adjustedValue = value.toDouble * factor
          writer.println(s"$time,$adjustedValue")
        }
      }
    }
  }

  case class TimeSeriesData(startTime: String, value: Double)
  case class ApiResponse(data: List[TimeSeriesData])

  mainMenu()
}
