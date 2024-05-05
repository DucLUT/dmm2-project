package reps.control
import scala.io.Source

import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter}

// Functor concept implementation
object Functor {
  // Functor trait with map function
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // Functor instance for Option
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
}

object RenewableControl {
  import Functor._

  // Renewable plant trait with shutdown operation
  trait RenewablePlant[A] {
    def shutdown(plant: A): A
    def turnOn(plant: A): A
  }

  trait Plant {
    def running: Boolean
  }

  // Renewable plant instance for Wind
  implicit val windPlant: RenewablePlant[Wind] = new RenewablePlant[Wind] {
    def shutdown(plant: Wind): Wind = {
      println("Shutting down wind plant...")
      // Additional shutdown logic can be added here
      plant.copy(running = false)
    }

    def turnOn(plant: Wind): Wind = {
      println("Turning on wind plant...")
      // Additional turn on logic can be added here
      plant.copy(running = true)
    }
  }

  // Renewable plant instance for Solar
  implicit val solarPlant: RenewablePlant[Solar] = new RenewablePlant[Solar] {
    def shutdown(plant: Solar): Solar = {
      println("Shutting down solar plant...")
      // Additional shutdown logic can be added here
      plant.copy(running = false)
    }

    def turnOn(plant: Solar): Solar = {
      println("Turning on solar plant...")
      // Additional turn on logic can be added here
      plant.copy(running = true)
    }
  }
  // Renewable plant instance for Hydro
  implicit val hydroPlant: RenewablePlant[Hydro] = new RenewablePlant[Hydro] {
    def shutdown(plant: Hydro): Hydro = {
      println("Shutting down hydro plant...")
      // Additional shutdown logic can be added here
      plant.copy(running = false)
    }

    def turnOn(plant: Hydro): Hydro = {
      println("Turning on hydro plant...")
      // Additional turn on logic can be added here
      plant.copy(running = true)
    }
  }
  // Wind plant case class
  case class Wind(name: String, running: Boolean = true) extends Plant

  // Solar plant case class
  case class Solar(name: String, running: Boolean = true) extends Plant

  // Hydro plant case class
  case class Hydro(name: String, running: Boolean = true) extends Plant

  // Function to interact with a specific renewable plant based on user choice
  def interactPlant[A <: Plant](plant: A)(implicit renewablePlant: RenewablePlant[A], functor: Functor[Option]): Option[A] = {
    println(s"Interacting with ${plant.getClass.getSimpleName}: ${plant.toString}")

    // Determine available operations based on the current status
    val availableOperations =
      if (plant.running) List("Shutdown")
      else List("Turn On")

    println("Choose an operation:")
    availableOperations.zipWithIndex.foreach { case (operation, index) =>
      println(s"${index + 1}. $operation")
    }
    print("Enter your choice: ")

    // Reading user input
    val userInput = scala.io.StdIn.readInt()
    val updatedPlant = userInput match {
      case 1 if plant.running => renewablePlant.shutdown(plant)
      case 1 if !plant.running => renewablePlant.turnOn(plant)
      case _ => plant
    }
    Some(updatedPlant)
  }

  // Function to read the status of the plants from a text file
  private def readPlantStatusFromFile(): Option[(Boolean, Boolean, Boolean)] = {
    Try {
      val source = Source.fromFile("plant_status.txt")
      val lines = source.getLines().toList
      source.close()
      val windStatus = lines.headOption.exists(_.split(":").last.trim == "true")
      val solarStatus = lines.lift(1).exists(_.split(":").last.trim == "true")
      val hydroStatus = lines.lift(2).exists(_.split(":").last.trim == "true")
      (windStatus, solarStatus, hydroStatus)
    } match {
      case Success(status) => Some(status)
      case Failure(_) => None
    }
  }

  // Function to write the status of the plants to a text file
  private def writePlantStatusToFile(windStatus: Boolean, solarStatus: Boolean, hydroStatus: Boolean): Unit = {
    val writer = new PrintWriter(new File("plant_status.txt"))
    writer.write(s"Wind Plant: $windStatus\n")
    writer.write(s"Solar Plant: $solarStatus\n")
    writer.write(s"Hydro Plant: $hydroStatus\n")
    writer.close()
  }


  // Run the Renewable Control application
  def runRenewableControlApp(): Unit = {
    // Read the status from the file if it exists
    val initialStatus = readPlantStatusFromFile()

    // Creating wind, solar, and hydro plants with initial status if available
    val (windStatus, solarStatus, hydroStatus) = initialStatus.getOrElse((true, true, true))
    val windPlant = Wind("Wind Plant", windStatus)
    val solarPlant = Solar("Solar Plant", solarStatus)
    val hydroPlant = Hydro("Hydro Plant", hydroStatus)

    // User interaction
    println("Choose a renewable plant to interact with:")
    println("1. Wind Plant")
    println("2. Solar Plant")
    println("3. Hydro Plant")
    print("Enter your choice (1, 2, or 3): ")

    // Reading user input
    val userInput = scala.io.StdIn.readInt()
    val result = userInput match {
      case 1 => interactPlant(windPlant)
      case 2 => interactPlant(solarPlant)
      case 3 => interactPlant(hydroPlant)
      case _ => None
    }

    // Handling result
    result match {
      case Some(updatedPlant: Wind) =>
        // Write the updated status to the file
        writePlantStatusToFile(updatedPlant.running, updatedPlant.running, updatedPlant.running)
        println(s"Updated plant: ${updatedPlant.name}, Running: ${updatedPlant.running}")
      case Some(updatedPlant: Solar) =>
        // Write the updated status to the file
        writePlantStatusToFile(updatedPlant.running, updatedPlant.running, updatedPlant.running)
        println(s"Updated plant: ${updatedPlant.name}, Running: ${updatedPlant.running}")
      case Some(updatedPlant: Hydro) =>
        // Write the updated status to the file
        writePlantStatusToFile(updatedPlant.running, updatedPlant.running, updatedPlant.running)
        println(s"Updated plant: ${updatedPlant.name}, Running: ${updatedPlant.running}")
      case None => println("Invalid choice or operation failed.")
    }
  }
}