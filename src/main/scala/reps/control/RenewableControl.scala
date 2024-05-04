package reps.control

// Importing Option type to handle possible absence of values
import scala.util.{Try, Success, Failure}

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
  }

  // Renewable plant instance for Wind
  implicit val windPlant: RenewablePlant[Wind] = new RenewablePlant[Wind] {
    def shutdown(plant: Wind): Wind = {
      println("Shutting down wind plant...")
      // Additional shutdown logic can be added here
      plant.copy(running = false)
    }
  }

  // Renewable plant instance for Solar
  implicit val solarPlant: RenewablePlant[Solar] = new RenewablePlant[Solar] {
    def shutdown(plant: Solar): Solar = {
      println("Shutting down solar plant...")
      // Additional shutdown logic can be added here
      plant.copy(running = false)
    }
  }

  // Wind plant case class
  case class Wind(name: String, running: Boolean = true)

  // Solar plant case class
  case class Solar(name: String, running: Boolean = true)

  // Function to interact with a specific renewable plant based on user choice
  def interactPlant[A](plant: A)(implicit renewablePlant: RenewablePlant[A], functor: Functor[Option]): Option[A] = {
    println(s"Interacting with ${plant.getClass.getSimpleName}: ${plant.toString}")
    println("Performing shutdown operation...")
    val shutdownResult = Try(renewablePlant.shutdown(plant))
    shutdownResult match {
      case Success(shutdownPlant) =>
        println("Shutdown operation successful.")
        Some(shutdownPlant)
      case Failure(exception) =>
        println(s"Failed to shutdown the plant: ${exception.getMessage}")
        None
    }
  }

  // Main function to demonstrate the application
  def main(args: Array[String]): Unit = {
    // Creating wind and solar plants
    val windPlant = Wind("Wind Plant")
    val solarPlant = Solar("Solar Plant")

    // User interaction
    println("Choose a renewable plant to interact with:")
    println("1. Wind Plant")
    println("2. Solar Plant")
    print("Enter your choice (1 or 2): ")

    // Reading user input
    val userInput = scala.io.StdIn.readInt()
    val result = userInput match {
      case 1 => interactPlant(windPlant)
      case 2 => interactPlant(solarPlant)
      case _ => None
    }

    // Handling result
    result match {
      case Some(updatedPlant) => println(s"Updated plant: $updatedPlant")
      case None => println("Invalid choice or operation failed.")
    }
  }
}
