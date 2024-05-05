import scala.util.{Try, Success, Failure}
import scala.io.Source
import java.io.{File, PrintWriter}



object RenewableControl {

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

  import Functor._

  // Renewable plant trait with shutdown and turnOn operations
  trait RenewablePlant[A] {
    def shutdown(plant: A): A
    def turnOn(plant: A): A
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
  case class Wind(name: String, running: Boolean = true)

  // Solar plant case class
  case class Solar(name: String, running: Boolean = true)

  // Hydro plant case class
  case class Hydro(name: String, running: Boolean = true)

  // Function to interact with a specific renewable plant based on user choice
  def interactPlant[A](plant: A)(implicit renewablePlant: RenewablePlant[A], functor: Functor[Option]): Option[A] = {
    println(s"Interacting with ${plant.getClass.getSimpleName}: ${plant.toString}")
    println("Choose an operation:")
    println("1. Shutdown")
    println("2. Turn On")
    print("Enter your choice (1 or 2): ")

    // Reading user input
    val userInput = scala.io.StdIn.readInt()
    val updatedPlant = userInput match {
      case 1 => renewablePlant.shutdown(plant)
      case 2 => renewablePlant.turnOn(plant)
      case _ => plant
    }
    Some(updatedPlant)
  }

  // Function to store the status of the plants in a text file
  def storePlantStatus(wind: Wind, solar: Solar, hydro: Hydro): Unit = {
    val writer = new PrintWriter(new File("plant_status.txt"))
    writer.write(s"Wind Plant: ${wind.running}\n")
    writer.write(s"Solar Plant: ${solar.running}\n")
    writer.write(s"Hydro Plant: ${hydro.running}\n")
    writer.close()
  }

  // Function to read the status of the plants from the text file
  def readPlantStatus(): Option[(Wind, Solar, Hydro)] = {
    Try {
      val lines = Source.fromFile("plant_status.txt").getLines().toList
      val windStatus = lines.find(_.startsWith("Wind Plant:")).map(_.split(":")(1).trim.toBoolean)
      val solarStatus = lines.find(_.startsWith("Solar Plant:")).map(_.split(":")(1).trim.toBoolean)
      val hydroStatus = lines.find(_.startsWith("Hydro Plant:")).map(_.split(":")(1).trim.toBoolean)
      (windStatus, solarStatus, hydroStatus)
    } match {
      case Success((Some(wind), Some(solar), Some(hydro))) => Some((Wind("Wind Plant", wind), Solar("Solar Plant", solar), Hydro("Hydro Plant", hydro)))
      case _ => None
    }
  }
}
