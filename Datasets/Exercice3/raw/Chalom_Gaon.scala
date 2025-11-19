
import scala.io.Source
import scala.io.StdIn.{readDouble, readInt, readLine}
import scala.util.Random
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

object CoffeeMachineApp {

  case class CoffeeMachine(
      val id: Int,
      var pin: String,
      var milk: Int,
      var sugar: Int,
      var coffee: Int
  ) {
    def updateStock(item: String, quantity: Int): Boolean = {
      item match {
        case "coffee" if coffee + quantity >= 0 => coffee += quantity; true
        case "sugar" if sugar + quantity >= 0   => sugar += quantity; true
        case "milk" if milk + quantity >= 0     => milk += quantity; true
        case _                                  => false
      }
    }

    def showStock(): Unit = {
      println(s"Machine $id Stock:")
      println(s"   Coffee: $coffee g")
      println(f"   Milk: ${milk / 1000.0}%.2f L")
      println(s"   Sugar: $sugar g")
    }
  }

  def loadMachines(file: String): ArrayBuffer[CoffeeMachine] = {
    val machines = ArrayBuffer[CoffeeMachine]()
    try {
      val source = Source.fromFile(file)
      for (line <- source.getLines().drop(1)) {
        val Array(pin, milk, sugar, coffee) = line.split(",")
        machines += CoffeeMachine(
          machines.size + 1,
          pin,
          milk.toInt,
          sugar.toInt,
          coffee.toInt
        )
      }
      source.close()
    } catch {
      case _: Exception => println("Error loading machines."); sys.exit(1)
    }
    machines
  }

  def saveMachines(file: String, machines: ArrayBuffer[CoffeeMachine]): Unit = {
    try {
      val writer = new PrintWriter(file)
      writer.println("PIN,MILK,SUGAR,COFFEE")
      machines.foreach { m =>
        writer.println(s"${m.pin},${m.milk},${m.sugar},${m.coffee}")
      }
      writer.close()
    } catch {
      case _: Exception => println("Error saving machines.")
    }
  }

  def validatePin(machine: CoffeeMachine): Boolean = {
    val inputPin = readLine("Enter PIN: ")
    inputPin == machine.pin
  }

  def serveCustomer(machine: CoffeeMachine): Boolean = {
    println("Choose a drink:")
    println("1) Espresso - $2.00")
    println("2) Cappuccino - $2.50")
    println("3) Latte - $3.00")
    val choice = readInt()
    val (requiredCoffee, requiredMilk, requiredSugar, price) = choice match {
      case 1 => (8, 0, 0, 2.00)
      case 2 => (6, 100, 5, 2.50)
      case 3 => (8, 150, 10, 3.00)
      case _ => println("Invalid choice."); return false
    }

    if (
      machine.updateStock("coffee", -requiredCoffee) &&
      machine.updateStock("milk", -requiredMilk) &&
      machine.updateStock("sugar", -requiredSugar)
    ) {
      println(f"Your drink is ready! Please pay $$${price}%.2f.")
      true
    } else {
      println("Not enough ingredients. Please try another drink.")
      false
    }
  }

  def restock(machine: CoffeeMachine): Unit = {
    machine.showStock()
    println("Enter amounts to add:")
    val addCoffee = readInt()
    val addMilk = (readDouble() * 1000).toInt
    val addSugar = readInt()
    machine.updateStock("coffee", addCoffee)
    machine.updateStock("milk", addMilk)
    machine.updateStock("sugar", addSugar)
    println("Stock updated.")
  }

  def adminMenu(machine: CoffeeMachine): Unit = {
    println("1) Restock ingredients")
    println("2) Change PIN")
    val choice = readInt()
    choice match {
      case 1 => restock(machine)
      case 2 =>
        println("Enter new PIN:")
        machine.pin = readLine()
        println("PIN updated.")
      case _ => println("Invalid choice.")
    }
  }

  def main(args: Array[String]): Unit = {
    val machines = loadMachines("machines.csv")
    var running = true

    while (running) {
      println("1) Customer Mode")
      println("2) Admin Mode")
      println("3) Exit")
      val mode = readInt()

      mode match {
        case 1 =>
          println("Choose a machine (1-5):")
          val machineId = readInt() - 1
          if (machineId >= 0 && machineId < machines.length) {
            serveCustomer(machines(machineId))
          } else {
            println("Invalid machine.")
          }
        case 2 =>
          println("Choose a machine (1-5):")
          val machineId = readInt() - 1
          if (machineId >= 0 && machineId < machines.length) {
            if (validatePin(machines(machineId))) {
              adminMenu(machines(machineId))
            } else {
              println("Invalid PIN.")
            }
          } else {
            println("Invalid machine.")
          }
        case 3 =>
          saveMachines("machines.csv", machines)
          println("Goodbye!")
          running = false
        case _ => println("Invalid option.")
      }
    }
  }
}
