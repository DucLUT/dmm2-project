package reps.control

import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter}

object Functor {
  // Functor trait with map function
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
}

object RenewableControl {
  import Functor._

  trait RenewablePlant[A] {
    def shutdown(plant: A): A
    def turnOn(plant: A): A
    def rotate(plant: A, degrees: Int): A
    def energyCapacity(plant: A): Double
  }

  trait Plant {
    def running: Boolean
  }

  // Wind plant case class
  case class Wind(name: String, running: Boolean = true, orientation: Int = 0) extends Plant
  // Solar plant case class
  case class Solar(name: String, running: Boolean = true, orientation: Int = 0) extends Plant
  // Hydro plant case class
  case class Hydro(name: String, running: Boolean = true, orientation: Int = 0) extends Plant

  object Wind {
    implicit val windPlant: RenewablePlant[Wind] = new RenewablePlant[Wind] {
      def shutdown(plant: Wind): Wind = {
        println("Shutting down wind plant...")
        plant.copy(running = false)
      }

      def turnOn(plant: Wind): Wind = {
        println("Turning on wind plant...")
        plant.copy(running = true)
      }

      def rotate(plant: Wind, degrees: Int): Wind = {
        println(s"Rotating wind plant by $degrees degrees...")
        plant.copy(orientation = (plant.orientation + degrees) % 360)
      }

      def energyCapacity(plant: Wind): Double = Wind.energyCapacity(plant)
    }

    def energyCapacity(plant: Wind): Double = 100000000000.0 // example value
  }

  object Solar {
    implicit val solarPlant: RenewablePlant[Solar] = new RenewablePlant[Solar] {
      def shutdown(plant: Solar): Solar = {
        println("Shutting down solar plant...")
        plant.copy(running = false)
      }

      def turnOn(plant: Solar): Solar = {
        println("Turning on solar plant...")
        plant.copy(running = true)
      }

      def rotate(plant: Solar, degrees: Int): Solar = {
        println(s"Rotating solar plant by $degrees degrees...")
        plant.copy(orientation = (plant.orientation + degrees) % 360)
      }

      def energyCapacity(plant: Solar): Double = Solar.energyCapacity(plant)
    }

    def energyCapacity(plant: Solar): Double = 50000000000.0 // example value
  }

  object Hydro {
    implicit val hydroPlant: RenewablePlant[Hydro] = new RenewablePlant[Hydro] {
      def shutdown(plant: Hydro): Hydro = {
        println("Shutting down hydro plant...")
        plant.copy(running = false)
      }

      def turnOn(plant: Hydro): Hydro = {
        println("Turning on hydro plant...")
        plant.copy(running = true)
      }

      def rotate(plant: Hydro, degrees: Int): Hydro = {
        println("Rotation operation is not applicable for hydro plants.")
        plant
      }

      def energyCapacity(plant: Hydro): Double = Hydro.energyCapacity(plant)
    }

    def energyCapacity(plant: Hydro): Double = 200000000000.0 // example value
  }

  // Companion object for interacting with renewable plants
  object RenewablePlant {
    def interactPlant[A <: Plant](plant: A)(implicit renewablePlant: RenewablePlant[A], functor: Functor[Option]): Option[A] = {
      println(s"Interacting with ${plant.getClass.getSimpleName}: ${plant.toString}")

      // Determine available operations based on the current status
      val availableOperations =
        if (plant.running) List("Shutdown", "Rotate")
        else List("Turn On")

      println("Choose an operation:")
      availableOperations.zipWithIndex.foreach { case (operation, index) =>
        println(s"${index + 1}. $operation")
      }
      print("Enter your choice: ")

      // Reading user input
      val userInput = scala.io.StdIn.readInt()
      val updatedPlant = userInput match {
        case 1 if plant.running => Some(renewablePlant.shutdown(plant))
        case 1 if !plant.running => Some(renewablePlant.turnOn(plant))
        case 2 if plant.isInstanceOf[Wind] || plant.isInstanceOf[Solar] => Some(renewablePlant.rotate(plant, 30)) // Assuming a default rotation of 30 degrees
        case _ => Some(plant)
      }
      updatedPlant
    }
  }

  def readPlantStatusFromFile(): Option[(Boolean, Boolean, Boolean)] = {
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

  private def writePlantStatusToFile(windStatus: Boolean, solarStatus: Boolean, hydroStatus: Boolean): Unit = {
    val writer = new PrintWriter(new File("plant_status.txt"))
    writer.write(s"Wind Plant: $windStatus\n")
    writer.write(s"Solar Plant: $solarStatus\n")
    writer.write(s"Hydro Plant: $hydroStatus\n")
    writer.close()
  }

  def calculateSumFromCSV(fileName: String): Double = {
    val source = Source.fromFile(fileName)
    val lines = source.getLines().toList
    source.close()
    lines.tail.map(_.split(",")(2).toDouble).sum
  }

  // Run the Renewable Control application
  def runRenewableControlApp(): Unit = {
    val initialStatus = readPlantStatusFromFile()

    val (windStatus, solarStatus, hydroStatus) = initialStatus.getOrElse((true, true, true))
    val windPlant = Wind("Wind Plant", windStatus)
    val solarPlant = Solar("Solar Plant", solarStatus)
    val hydroPlant = Hydro("Hydro Plant", hydroStatus)

    println("Choose a renewable plant to interact with:")
    println("1. Wind Plant")
    println("2. Solar Plant")
    println("3. Hydro Plant")
    println("4. Calculate Sum/Capacity")
    println("5. Rotate Plant")
    print("Enter your choice (1, 2, 3, 4, or 5): ")

    val userInput = scala.io.StdIn.readInt()
    val result = userInput match {
      case 1 => RenewablePlant.interactPlant(windPlant)
      case 2 => RenewablePlant.interactPlant(solarPlant)
      case 3 => RenewablePlant.interactPlant(hydroPlant)
      case 4 => {
        val windSum = calculateSumFromCSV("data/wind.csv")
        val solarSum = calculateSumFromCSV("data/solar.csv")
        val hydroSum = calculateSumFromCSV("data/hydro.csv")

        val windCapacity = Wind.energyCapacity(windPlant)
        val solarCapacity = Solar.energyCapacity(solarPlant)
        val hydroCapacity = Hydro.energyCapacity(hydroPlant)

        println(s"Wind Plant: $windSum/$windCapacity")
        println(s"Solar Plant: $solarSum/$solarCapacity")
        println(s"Hydro Plant: $hydroSum/$hydroCapacity")

        if (windSum > windCapacity) {
          println("Alert: Wind energy production exceeds capacity. Turning off wind plant.")
          val updatedWindPlant = Wind.windPlant.shutdown(windPlant)
          writePlantStatusToFile(updatedWindPlant.running, solarPlant.running, hydroPlant.running)
          None // Returning None to avoid triggering the 'Updated plant' message
        } else if (solarSum > solarCapacity) {
          println("Alert: Solar energy production exceeds capacity. Turning off solar plant.")
          val updatedSolarPlant = Solar.solarPlant.shutdown(solarPlant)
          writePlantStatusToFile(windPlant.running, updatedSolarPlant.running, hydroPlant.running)
          None // Returning None to avoid triggering the 'Updated plant' message
        } else if (hydroSum > hydroCapacity) {
          println("Alert: Hydro energy production exceeds capacity. Turning off hydro plant.")
          val updatedHydroPlant = Hydro.hydroPlant.shutdown(hydroPlant)
          writePlantStatusToFile(windPlant.running, solarPlant.running, updatedHydroPlant.running)
          None // Returning None to avoid triggering the 'Updated plant' message
        } else {
          None
        }
      }
      case 5 => {
        println("Choose a plant to rotate:")
        println("1. Wind Plant")
        println("2. Solar Plant")
        print("Enter your choice (1 or 2): ")
        val rotateChoice = scala.io.StdIn.readInt()
        rotateChoice match {
          case 1 => Some(Wind.windPlant.rotate(windPlant, 30)) // Assuming a default rotation of 30 degrees
          case 2 => Some(Solar.solarPlant.rotate(solarPlant, 30)) // Assuming a default rotation of 30 degrees
          case _ => None
        }
      }
      case _ => None
    }

    result match {
      case Some(updatedPlant: Wind) =>
        writePlantStatusToFile(updatedPlant.running, updatedPlant.running, updatedPlant.running)
        println(s"Updated plant: ${updatedPlant.name}, Running: ${updatedPlant.running}")
      case Some(updatedPlant: Solar) =>
        writePlantStatusToFile(updatedPlant.running, updatedPlant.running, updatedPlant.running)
        println(s"Updated plant: ${updatedPlant.name}, Running: ${updatedPlant.running}")
      case Some(updatedPlant: Hydro) =>
        writePlantStatusToFile(updatedPlant.running, updatedPlant.running, updatedPlant.running)
        println(s"Updated plant: ${updatedPlant.name}, Running: ${updatedPlant.running}")
      case None =>
    }
  }
}
