package DMM2

import scala.io.StdIn


object MainApp {
  def main(args: Array[String]): Unit = {
    menu()
  }

  def menu(): Unit = {
    println("\nSelect an option:")
    println("1. Monitor Energy")
    println("2. Collect Data")
    println("3. Energy Management System")
    println("4. Run Energy Data Processor")
    println("5. Run Renewable Energy Monitoring System")
    println("6. Exit")

    StdIn.readLine() match {
      case "1" =>
        monitor_energy.main(Array.empty)
        menu()
      case "2" =>
        collect.main(Array.empty)
        menu()
      case "3" =>
        EnergyManagementSystem.main(Array.empty)
        menu()
      case "4" =>
        EnergyDataProcessor.main(Array.empty)
        menu()
      case "5" =>
        EnergyAlertSystem.main(Array.empty)
        menu()
      case "6" =>
        println("Exiting...")
      case _ =>
        println("Invalid choice")
        menu()
    }
  }
}
